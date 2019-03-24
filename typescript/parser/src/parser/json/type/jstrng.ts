import { JValue } from './jvalue'

export class JString extends JValue {
	value: string;

	constructor(value: string) {
		super()
		this.value = value
	}

	equals(other: any) {
		return other instanceof JString && this.value == other.value
	}
}
