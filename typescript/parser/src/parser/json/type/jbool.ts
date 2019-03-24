import { JValue } from './jvalue'

export class JBool extends JValue {
	value: boolean;

	constructor(value: boolean) {
		super()
		this.value = value
	}

	equals(other: any) {
		return other instanceof JBool && this.value == other.value
	}
}
