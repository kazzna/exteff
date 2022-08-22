package ext

import org.scalatest.freespec.AnyFreeSpec

class NaturalTransformationSpec extends AnyFreeSpec {
  "A NaturalTransformation" - {
    val listToOption: List ~> Option = new NaturalTransformation[List, Option] {
      override def apply[A](fa: List[A]): Option[A] = fa.headOption
    }

    val optionToList: Option ~> List = new NaturalTransformation[Option, List] {
      override def apply[A](fa: Option[A]): List[A] = fa.toList
    }

    "should transform container class" in {
      val input = List(42)
      val expected1: Option[Int] = Some(42)

      assert(listToOption(input) === expected1)
    }

    "should compose transformations" in {
      val input = List(42)
      val nt1 = listToOption.compose(optionToList)
      val nt2 = nt1.compose(nt1).compose(nt1.compose(nt1))
      val nt3 = nt2.compose(nt2.compose(nt2).compose(nt2.compose(nt2)))

      assert(nt3(input) === input)
    }

    "reflect" - {
      "should be ignored when composed" in {
        val nt = listToOption

        assert(nt.compose(NaturalTransformation.reflect) === nt)
        assert(NaturalTransformation.reflect.compose(nt) === nt)

        val o2o = NaturalTransformation.reflect[Option]
        assert(o2o.compose(o2o) === o2o)
      }
    }
  }
}
