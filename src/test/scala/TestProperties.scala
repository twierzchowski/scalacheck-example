import org.scalacheck._
import org.scalacheck.Prop._
import Gen._

object CipherProperties extends Properties("cipher properties") {
	property("roundtrip") = forAll { (a:String, b:Int) =>
	UsefulMethods.cipher(UsefulMethods.cipher(a,b),-b) == a }

	val newGenerator: Gen[(String, Int)] = for {
	str <- Gen.alphaStr
	shift <- Gen.choose(0,10)
	} yield (str, shift)

	property("roundtrip alfastrings") = forAll(newGenerator) { input: (String, Int) =>
	UsefulMethods.cipher(UsefulMethods.cipher(input._1,input._2),-input._2) == input._1 }
}

object TestProperties {

	def main(args: Array[String]) {
		println("Hello world!")
		CipherProperties.check
	}
}
