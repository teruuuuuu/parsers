export interface ParseResult<T> {}

export class ParseSuccess<T> implements ParseResult<T> {
    value: T;
    next: string;

    constructor(value: T, next: string){
        this.value = value
        this.next = next
    }

    public toString(): string{
        return "Success(" + this.value + ", " + this.next + ")";
    }
}

export class ParseFailer<T> implements ParseResult<T> {
    message: string;
    next: string;

    constructor(message: string, next: string){
        this.message = message
        this.next = next
    }

    public toString(): string{
        return "Failer(" + this.message + ", " + this.next + ")";
    }
}

