package ext.transform

import ext.NaturalTransformation
import test.Box

object BoxToOption extends NaturalTransformation[Box, Option] {
  override def apply[A](fa: Box[A]): Option[A] = Some(fa.value)
}
