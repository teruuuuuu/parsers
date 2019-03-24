export class None{}
export class Some<T> {
	value: T
	constructor(value: T) {
		this.value = value
	}
}
export type Option<T> = None | Some<T>
