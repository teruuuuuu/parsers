export abstract class JValue {

	equals(other: any){
		return other instanceof JValue
	}
}
