package scalaz

object S {
  implicit def id[A](x: A) = Identity.id(x)
  
  implicit def ArrayByteTo(bs: Array[Byte]) = ArrayByte.ArrayByteTo(bs)

  implicit def ArrayByteFrom(bs: ArrayByte) = ArrayByte.ArrayByteFrom(bs)

  implicit def BooleanTo(b: Boolean) = BooleanW.BooleanTo(b)

  implicit def BooleanFrom(b: BooleanW) = BooleanW.BooleanFrom(b)

  implicit def BooleanConjunctionFrom(b: BooleanConjunction) = BooleanConjunction.BooleanConjunctionFrom(b)

  implicit def CharTo(c: Char) = CharW.CharTo(c)

  implicit def CharFrom(c: CharW) = CharW.CharFrom(c)

  implicit def DigitLong(d: Digit) = Digit.DigitLong(d)

  implicit def LongDigit(n: Long) = Digit.LongDigit(n)

  implicit def Function1To[T, R](f: T => R) = Function1W.Function1To(f)

  implicit def Function1From[T, R](f: Function1W[T, R]) = Function1W.Function1From(f)

  implicit def Function2To[T1, T2, R](f: (T1, T2) => R) = Function2W.Function2To(f)

  implicit def Function2From[T1, T2, R](f: Function2W[T1, T2, R]) = Function2W.Function2From(f)

  implicit def IterableTo[A](i: Iterable[A]) = IterableW.IterableTo(i)

  implicit def IterableFrom[A](i: IterableW[A]) = IterableW.IterableFrom(i)

  implicit def JavaIterableTo[A](i: java.lang.Iterable[A]): IterableW[A] = IterableW.JavaIterableTo(i)

  implicit def ListTo[A](as: List[A]): ListW[A] = ListW.ListTo(as)

  implicit def ListFrom[A](as: ListW[A]) = ListW.ListFrom(as)

  implicit def LongTo(n: Long) = LongW.LongTo(n)

  implicit def LongFrom(n: LongW) = LongW.LongFrom(n)

  implicit def OptionTo[A](o: Option[A]) = OptionW.OptionTo(o)

  implicit def OptionFrom[A](o: OptionW[A]) = OptionW.OptionFrom(o)

  implicit def StringTo(ss: String) = StringW.StringTo(ss)

  implicit def StringFrom(s: StringW) = StringW.StringFrom(s)

}