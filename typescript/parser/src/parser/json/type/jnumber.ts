import { JValue } from './jvalue'

export class JNumber extends JValue {
	value: number;

	constructor(value: number) {
		super()
		this.value = value
	}

	equals(other: any) {
		return other instanceof JNumber && this.value == other.value
	}
}
