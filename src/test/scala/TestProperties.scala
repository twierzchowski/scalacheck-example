import org.scalacheck._
import org.scalacheck.Prop._
import Gen._

object CipherProperties extends Properties("cipher properties") {
	

//	property("roundtrip") = forAll { (a:String, b:Int) =>
//	UsefulMethods.cipher(UsefulMethods.cipher(a,b),-b) == a }

	def LowerAlphaStr: Gen[String] =
		listOf(alphaLowerChar).map(_.mkString).suchThat(_.forall(_.isLetter))

	val newGenerator: Gen[(String, Int)] = for {
		str <- LowerAlphaStr
		shift <- Gen.choose(-10000,10000)
	} yield (str, shift)

	def isAlphaString(s:String): Boolean = {
		for(chr <- s)
			if (chr<'a' || chr>'z')
				return false
		return true
	}

	property("encrypted text should be alphastring") = forAll(newGenerator) { input: (String, Int) =>
	isAlphaString(UsefulMethods.cipher(input._1, input._2)) == true
	}
	
	property("roundtrip alfastrings") = forAll(newGenerator) { input: (String, Int) =>
	UsefulMethods.cipher(UsefulMethods.cipher(input._1,input._2),-input._2) == input._1 
	}
}

object TestProperties {

	def main(args: Array[String]) {
		println("Hello world!")
		CipherProperties.check
	}
}
