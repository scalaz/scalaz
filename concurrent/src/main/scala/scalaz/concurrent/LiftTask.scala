package scalaz
package concurrent

import scalaz.effect.{LiftIO, IO}

trait LiftTask[F[_]] extends LiftIO[F] {
  
  // what we're saying here is that Task is basically the same as IO
  def liftIO[A](ioa: IO[A]): F[A] =
    liftTask(Task delay { ioa.unsafePerformIO() })
  
  def liftTask[A](task: Task[A]): F[A]
}

object LiftTask {
  @inline
  def apply[F[_]](implicit F: LiftTask[F]): LiftTask[F] = F
  
  implicit def idTLiftTask[F[_]: LiftTask] = new LiftTask[({type λ[α]=IdT[F, α]})#λ] {
    def liftTask[A](task: Task[A]) = IdT(LiftTask[F].liftTask(task))
  }

  implicit def listTLiftTask[F[_]: LiftTask] = new LiftTask[({type λ[α]=ListT[F, α]})#λ] {
    def liftTask[A](task: Task[A]) = ListT(LiftTask[F].liftTask(task.map(_ :: Nil)))
  }

  implicit def optionTLiftTask[F[_]: LiftTask] = new LiftTask[({type λ[α]=OptionT[F, α]})#λ] {
    def liftTask[A](task: Task[A]) = OptionT(LiftTask[F].liftTask(task.map(Some(_): Option[A])))
  }

  implicit def eitherTLiftTask[F[_]: LiftTask, E] = new LiftTask[({type λ[α]=EitherT[F, E, α]})#λ] {
    def liftTask[A](task: Task[A]) = EitherT(LiftTask[F].liftTask(task.map(\/.right)))
  }

  implicit def streamTLiftTask[F[_]: LiftTask: Applicative] = new LiftTask[({type λ[α]=StreamT[F, α]})#λ] {
    def liftTask[A](task: Task[A]) = StreamT(LiftTask[F].liftTask(task.map(StreamT.Yield(_, StreamT.empty))))
  }

  implicit def kleisliLiftTask[F[_]: LiftTask, E] = new LiftTask[({type λ[α]=Kleisli[F, E, α]})#λ] {
    def liftTask[A](task: Task[A]) = Kleisli(_ => LiftTask[F].liftTask(task))
  }

  implicit def writerTLiftTask[F[_]: LiftTask, W: Monoid] = new LiftTask[({type λ[α]=WriterT[F, W, α]})#λ] {
    def liftTask[A](task: Task[A]) = WriterT(LiftTask[F].liftTask(task.map((Monoid[W].zero, _))))
  }

  implicit def stateTLiftTask[F[_]: LiftTask, S] = new LiftTask[({type λ[α]=StateT[F, S, α]})#λ] {
    def liftTask[A](task: Task[A]) = StateT(s => LiftTask[F].liftTask(task.map((s, _))))
  }
}
