import { JValue } from './jvalue'
import { JString } from './jstrng';

export class JObject extends JValue {
	value: Map<JString, JValue>;

	constructor(value: Map<JString, JValue>) {
		super()
		this.value = value
	}

	equals(other: any) {
		if(!(other instanceof JObject) && other.value.keys.length != this.value.keys.length) {
			return false;
		} else {
			const otherValue: Map<JString, JValue> = other.value
			this.value.forEach((value, key) => {
				if(!otherValue.has(key) || !value.equals(otherValue.get(key))){
					return false;
				}
				return true;
			})
		}

	}
}
