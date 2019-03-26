import { JValue } from './jvalue'

export class JArray extends JValue {
	value: JValue[];

	constructor(value: JValue[]) {
		super()
		this.value = value
	}

	equals(other: any) {
		if(!(other instanceof JArray) && other.value.length != this.value.length) {
			return false;
		} else {
			let result = true;
			this.value.forEach((v, index) => {
				if(!v.equals(other.value[index])){
					result = false;
				}
			})
			return result;
		}

	}
}
