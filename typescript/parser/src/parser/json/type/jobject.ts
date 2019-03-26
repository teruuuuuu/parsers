import { JValue } from './jvalue'
import { JString } from './jstrng';
import { Option, None, Some } from '../../common/type/option';

export class JObject extends JValue {
	value: Map<JString, JValue>;

	constructor(value: Map<JString, JValue>) {
		super()
		this.value = value
	}

	get(key: JString):Option<JValue> {
		let ret: Option<JValue> = None
		this.value.forEach((v, k) => {
			if(k.equals(key)) {
				ret = new Some(v)
			}
		})
		return ret
	}

	equals(other: any) {
		if(!(other instanceof JObject) && other.value.keys.length != this.value.keys.length) {
			return false;
		} else {
			let result = true;
			this.value.forEach((v, k) => {
				const otherV = other.get(k)
				if(!(otherV instanceof Some) || !v.equals(otherV.value)){
					result = false;
				}
			})
			return result;
		}
	}
}
