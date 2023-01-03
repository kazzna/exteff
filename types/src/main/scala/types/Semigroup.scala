package types

trait Semigroup[A] {
  def append: (A, A) => A
}
