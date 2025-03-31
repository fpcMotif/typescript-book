---
recall: asterisk
---


# Test for `recall`

* eee :: 1234
* fdffdd :: 333

# TypeScript Concepts Flashcards

## Basic Types and Errors

* What is the difference between `length` property and `length()` method on strings in TypeScript?
? :: `length` is a property that returns the number of characters in a string. It should be accessed without parentheses: `firstName.length`.

`length()` doesn't exist on strings and attempting to use it will cause a TypeScript error: "This expression is not callable. Type 'Number' has no call signatures."

```typescript
// Correct
const nameLength = firstName.length;

// Incorrect - will cause a type error
const nameLength = firstName.length();
```

What happens if you pass more arguments than defined parameters to a function in TypeScript?
?
TypeScript will throw an error: "Expected X arguments, but got Y."

```typescript
function sayMyName(fullName) {
  console.log(`You acting kind of shady, ain't callin' me ${fullName}`);
}

// Error: Expected 1 argument, but got 2
sayMyName("Beyoncé", "Knowles");
```

How does TypeScript handle return types for functions with explicit return type annotations?
?
If a function has an explicit return type other than `void`, `undefined`, or `any`, TypeScript will require the function to return a value of that type.

```typescript
// This will cause an error because the function doesn't return a boolean
function paintPainting(painter: Painter, painting: string): boolean { /* ... */ }
// Error: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
```

## Interfaces and Type Usage

What's the correct way to define an interface for objects that have methods in TypeScript?
?
Use the method signature syntax in the interface:

```typescript
interface Painter {
  finish(): boolean;
  ownMaterials: Material[];
  paint(painting: string, materials: Material[]): boolean;
}
```

Can you use an interface name as a value in TypeScript?
?
No. Interfaces only exist at compile-time for type checking and are not available at runtime.

```typescript
// Error: 'Painter' only refers to a type, but is being used as a value here
Painter.paint("The Starry Night", ["oil", "canvas"]);
```

## Type Inference and Union Types

How does type inference work with conditional (ternary) expressions in TypeScript?
?
TypeScript infers the type that covers all possible values from the conditional branches.

```typescript
// Inferred type: string
let bestSong = Math.random() > 0.5
  ? "Chain of Fools"
  : "Respect";
```

How do you define a union type in TypeScript?
?
Use the pipe symbol (`|`) between types:

```typescript
let rocker: string | number; // Type can be either string or number
rocker = "Joan Jett";    // OK
rocker = 19.58;          // OK
```

How can you safely call methods specific to one type within a union type?
?
Use type guards to narrow down the type:

```typescript
let rocker: string | number;
rocker = "Joan Jett";

// Check if rocker is a string before calling string-specific methods
if (typeof rocker === "string") {
  rocker.toUpperCase(); // Now TypeScript knows this is safe
}
```

## Object Properties

What happens when you try to add a property to an object that wasn't defined in its initial structure?
?
By default, TypeScript will throw an error if you try to add properties that weren't initially defined:

```typescript
let cher = {
  firstName: "Cherilyn",
  lastName: "Sarkisian",
};

// Error: Property 'middleName' does not exist on type '{ firstName: string; lastName: string; }'
cher.middleName = "Sarkisian";
```

## Literal Types

What are literal types in TypeScript?
?
Literal types are specific value types rather than general types like `string` or `number`. They represent exactly one value:

```typescript
// Type is specifically the string "Ada", not any string
let specificallyAda: "Ada";
specificallyAda = "Ada"; // OK
specificallyAda = "Byron"; // Error: Type '"Byron"' is not assignable to type '"Ada"'
```

How are string literals different from general string types in TypeScript?
?
A string literal type represents exactly one specific string value, while the string type represents any possible string:

```typescript
let specificallyAda: "Ada"; // Can only be the string "Ada"
let someString: string = ""; // Can be any string
specificallyAda = someString; // Error: Type 'string' is not assignable to type '"Ada"'
```

## Null and Undefined Handling

How does TypeScript handle operations on a value that might be undefined?
?
TypeScript will show an error if you try to access properties or methods on a value that might be undefined:

```typescript
let nameMaybe = Math.random() > 0.5
  ? "Tony Hoare"
  : undefined;

// Error: 'nameMaybe' is possibly 'undefined'
nameMaybe.toLowerCase();
```

What is the optional chaining operator and how does it help with undefined values?
?
The optional chaining operator (`?.`) allows you to safely access properties or methods on objects that might be undefined or null, without causing runtime errors:

```typescript
let mathematicianm: string | undefined;
mathematicianm?.length; // OK: Returns undefined if mathematicianm is undefined
```

How can you handle potentially undefined values using conditional statements?
?
You can use an `if` statement to check if a value exists before accessing its properties:

```typescript
let geneticist = Math.random() > 0.5
  ? "Barbara McClintock"
  : undefined;

if (geneticist) {
  geneticist.toUpperCase(); // OK: string type is guaranteed here
}
```

## Type Assignability

What rules does TypeScript follow for assignment compatibility?
?
TypeScript only allows assigning values to variables if the value's type is compatible with the variable's type:

```typescript
let lastName = "King"; // Type inferred as string
lastName = true; // Error: Type 'boolean' is not assignable to type 'string'
```

What is the 'any' type in TypeScript and why should it be avoided?
?
The `any` type effectively disables type checking for a variable. Values of type `any` can:
- Be assigned any value
- Have any property accessed (even non-existent ones)
- Have any method called (even non-existent ones)

```typescript
let rocker: any; // Type: any
rocker = "Joan Jett"; // OK
rocker = 19.58; // OK
rocker.toUpperCase(); // OK but might crash at runtime
rocker.nonExistentMethod(); // TypeScript won't catch this error
```

It should be avoided because it defeats the purpose of using TypeScript for type safety. 