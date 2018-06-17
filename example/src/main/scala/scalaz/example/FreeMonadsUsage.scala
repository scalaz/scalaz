package scalaz.example

import scalaz._, Scalaz._

// example of using the Monad extensions in a Free program.
// For an example of Free on its own, see FreeUsage.
object FreeMonadsUsage {

  // a test algebra...
  trait Console[F[_]] {
    def read: F[String]
    def write(s: String): F[Unit]
  }
  object Console {
    // with a data type instruction set
    sealed abstract class Ast[A]
    final case class Read() extends Ast[String]
    final case class Write(s: String) extends Ast[Unit]

    // and an implementation of the trait for any Free algebra
    def liftF[F[_]](implicit I: Ast :<: F): Console[Free[F, ?]] =
      new Console[Free[F, ?]] {
        def read: Free[F, String] = Free.liftF(I.inj(Read()))
        def write(s: String): Free[F, Unit] = Free.liftF(I.inj(Write(s)))
      }
  }

  // "business logic" that uses the MTL typeclasses. The utility of what these
  // methods are doing is irrelevant, the only important thing is that it uses
  // methods from both the specialised Monad and the Console algebra.
  def withState[F[_]](C: Console[F])(implicit F: MonadState[F, String]): F[String] =
    for {
      was <- F.get
      in  <- C.read
      update = was + in
      _   <- F.put(update)
    } yield update

  def withError[F[_]](C: Console[F])(implicit F: MonadError[F, String]): F[Int] =
    for {
      in <- C.read
      parsed = in.parseInt
      i <- parsed.fold(_ => F.raiseError(s"bad input '$in'"), F.pure(_))
    } yield i

  def withReader[F[_]](C: Console[F])(implicit F: MonadReader[F, String]): F[String] =
    for {
      in <- C.read
      path <- F.local(p => s"$p/$in")(F.ask)
    } yield path

  def withPlus[F[_]](C: Console[F])(implicit F: MonadPlus[F]): F[Int] =
    for {
     in <- C.read
     parsed = in.parseInt
     i <- parsed.fold(_ => F.empty, F.pure(_))
   } yield i

  def withWriter[F[_]](C: Console[F])(implicit F: MonadListen[F, String]): F[String] =
   for {
     in <- C.read
     _ <- F.tell(in)
     out <- F.listen(F.pure(()))
   } yield out._2

  // now show that each can be converted into a Free program. Implementing the
  // interpreters is left as an exercise to the reader. Unfortunately there is a
  // lot of boilerplate here because implicit search tends to fail without the
  // SI-2712 fix.

  def freeState = {
    type StateAst[a] = MonadState.Ast[String, a]
    type Ast[a] = Coproduct[StateAst, Console.Ast, a]

    // scala implicit search fail... needs a helping hand without SI-2712
    implicit val injecter: StateAst :<: Ast = Inject.leftInjectInstance

    type F[a] = Free[Ast, a]
    // these generators are not implicit by default for compile perf
    implicit val monad: MonadState[F, String] = MonadState.liftF

    withState[F](Console.liftF)
  }

  def freeError = {
    type ErrorAst[a] = MonadError.Ast[String, a]
    type Ast[a] = Coproduct[ErrorAst, Console.Ast, a]
    implicit val injecter: ErrorAst :<: Ast = Inject.leftInjectInstance

    type F[a] = Free[Ast, a]
    implicit val monad: MonadError[F, String] = MonadError.liftF

    withError[F](Console.liftF)
  }

  def freeReader = {
    type ReaderAst[a] = MonadReader.Ast[String, a]
    type Ast[a] = Coproduct[ReaderAst, Console.Ast, a]
    implicit val injecter: ReaderAst :<: Ast = Inject.leftInjectInstance

    type F[a] = Free[Ast, a]
    implicit val monad: MonadReader[F, String] = MonadReader.liftF

    withReader[F](Console.liftF)
  }

  def freePlus = {
    type Ast[a] = Coproduct[MonadPlus.Ast, Console.Ast, a]
    implicit val injecter: MonadPlus.Ast :<: Ast = Inject.leftInjectInstance

    type F[a] = Free[Ast, a]
    implicit val monad: MonadPlus[F] = MonadPlus.liftF

    withPlus[F](Console.liftF)
  }

  def freeWriter = {
    type TellAst[a] = MonadTell.Ast[String, a]
    type PartialAst[a] = Coproduct[TellAst, Console.Ast, a]
    type ListenAst[a] = MonadListen.Ast[String, a]
    type Ast[a] = Coproduct[ListenAst, PartialAst, a]

    // omg SI-2712...
    implicit val injector0: Console.Ast :<: PartialAst = Inject.rightInjectInstance
    implicit val injecter1: ListenAst :<: Ast = Inject.leftInjectInstance
    implicit val injecter2: TellAst :<: Ast = Inject.rightInjectInstance

    type F[a] = Free[Ast, a]
    implicit val monadL: MonadListen[F, String] = MonadListen.liftF

    withWriter[F](Console.liftF)
  }

}
