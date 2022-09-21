package ext.transform

import ext.NaturalTransformation

object OptionToList extends NaturalTransformation[Option, List] {
  override def apply[A](fa: Option[A]): List[A] = fa.toList
}
