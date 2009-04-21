package scalaz

object S {
  implicit def id[A](x: A) = Identity.id(x)
  
  implicit def ArrayByteTo(bs: Array[Byte]) = ArrayByte.ArrayByteTo(bs)

  implicit def ArrayByteFrom(bs: ArrayByte) = ArrayByte.ArrayByteFrom(bs)

  implicit def BooleanTo(b: Boolean) = BooleanW.BooleanTo(b)

  implicit def BooleanFrom(b: BooleanW) = BooleanW.BooleanFrom(b)

  def conjunction(b: Boolean) = BooleanConjunction.conjunction(b)

  implicit def BooleanConjunctionFrom(b: BooleanConjunction) = BooleanConjunction.BooleanConjunctionFrom(b)

  def multiplication(n: Byte) = ByteMultiplication.multiplication(n)

  implicit def ByteMultiplicationFrom(n: ByteMultiplication) = ByteMultiplication.ByteMultiplicationFrom(n)

  def multiplication(n: Char) = CharMultiplication.multiplication(n)

  implicit def CharMultiplicationFrom(n: CharMultiplication) = CharMultiplication.CharMultiplicationFrom(n)

  implicit def CharTo(c: Char) = CharW.CharTo(c)

  implicit def CharFrom(c: CharW) = CharW.CharFrom(c)

  implicit def DigitLong(d: Digit) = Digit.DigitLong(d)

  implicit def LongDigit(n: Long) = Digit.LongDigit(n)

  def multiplication(n: Long) = LongMultiplication.multiplication(n)

  implicit def LongMultiplicationFrom(n: LongMultiplication) = LongMultiplication.LongMultiplicationFrom(n)

  implicit def Function1To[T, R](f: T => R) = Function1W.Function1To(f)

  implicit def Function1From[T, R](f: Function1W[T, R]) = Function1W.Function1From(f)

  implicit def Function2To[T1, T2, R](f: (T1, T2) => R) = Function2W.Function2To(f)

  implicit def Function2From[T1, T2, R](f: Function2W[T1, T2, R]) = Function2W.Function2From(f)

  def multiplication(n: Int) = IntMultiplication.multiplication(n)

  implicit def IntMultiplicationFrom(n: IntMultiplication) = IntMultiplication.IntMultiplicationFrom(n)

  implicit def IterableTo[A](i: Iterable[A]) = IterableW.IterableTo(i)

  implicit def IterableFrom[A](i: IterableW[A]) = IterableW.IterableFrom(i)

  implicit def JavaIterableTo[A](i: java.lang.Iterable[A]): IterableW[A] = IterableW.JavaIterableTo(i)

  implicit def ListTo[A](as: List[A]): ListW[A] = ListW.ListTo(as)

  implicit def ListFrom[A](as: ListW[A]) = ListW.ListFrom(as)

  implicit def LongTo(n: Long) = LongW.LongTo(n)

  implicit def LongFrom(n: LongW) = LongW.LongFrom(n)

  implicit def OptionTo[A](o: Option[A]) = OptionW.OptionTo(o)

  implicit def OptionFrom[A](o: OptionW[A]) = OptionW.OptionFrom(o)

  def multiplication(n: Short) = ShortMultiplication.multiplication(n)

  implicit def ShortMultiplicationFrom(n: ShortMultiplication) = ShortMultiplication.ShortMultiplicationFrom(n)

  implicit def StringTo(ss: String) = StringW.StringTo(ss)

  implicit def StringFrom(s: StringW) = StringW.StringFrom(s)

}