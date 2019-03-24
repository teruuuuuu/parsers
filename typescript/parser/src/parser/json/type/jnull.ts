import { JValue } from './jvalue'

export class JNull extends JValue {
	equals(other: any) {
		return other instanceof JNull
	}
}
