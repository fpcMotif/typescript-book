/**
 * COMPARISON OF ERROR HANDLING APPROACHES IN TYPESCRIPT
 * 
 * This file compares different approaches for handling operations that might fail:
 * 1. Original approach (throws errors)
 * 2. Promise-based approach
 * 3. Option pattern (Some/None)
 * 4. Either pattern (Left/Right)
 * 5. Result pattern (Success/Failure)
 */

// Common User interface used across all approaches
interface User {
  ageInMonths: number;
  name: string;
}

// Sample data for examples
const users: User[] = [
  { ageInMonths: 36, name: 'Alice' },
  { ageInMonths: 48, name: 'Bob' }
];

// Empty array for testing error cases
const emptyUsers: User[] = [];

//==============================================================================
// APPROACH 1: ORIGINAL (THROWS ERRORS)
//==============================================================================

/**
 * Original approach - throws errors
 * Problems:
 * - Type signature (number) doesn't indicate potential errors
 * - Will crash at runtime if errors aren't caught
 */
function findUserAgeByName(users: User[], name: string): number {
  if (users.length === 0) {
    throw new Error('There are no users!');
  }
  
  const user = users.find(u => u.name === name);
  
  if (!user) {
    throw new Error(`User "${name}" not found!`);
  } 
  
  return user.ageInMonths;
}

// Example usage that will crash at runtime
function originalExample(): void {
  try {
    // This works fine
    const userAge1 = findUserAgeByName(users, 'Alice');
    console.log(`Alice is ${userAge1 / 12} years old!`);

    // This throws an error!
    const userAge2 = findUserAgeByName(emptyUsers, 'Bob');
    console.log(`Bob is ${userAge2 / 12} years old!`); // Never reaches this line
  } catch (error) {
    console.error('Caught error:', error);
  }
}

//==============================================================================
// APPROACH 2: PROMISE-BASED
//==============================================================================

/**
 * Promise-based approach - errors become rejected promises
 * Advantages:
 * - Type signature (Promise<number>) indicates potential for rejection
 * - Can use .then() and .catch() for handling both cases
 * - Works well with async/await
 */
function promiseFindUserAgeByName(users: User[], name: string): Promise<number> {
  if (users.length === 0) {
    return Promise.reject(new Error('There are no users!'));
  }
  
  const user = users.find(u => u.name === name);
  
  if (!user) {
    return Promise.reject(new Error(`User "${name}" not found!`));
  } 
  
  return Promise.resolve(user.ageInMonths);
}

// Example usage with promises
function promiseExample(): void {
  // This works fine
  promiseFindUserAgeByName(users, 'Alice')
    .then(age => console.log(`Alice is ${age / 12} years old!`))
    .catch(error => console.error('Error:', error.message));

  // This rejects, but our code doesn't crash
  promiseFindUserAgeByName(emptyUsers, 'Bob')
    .then(age => console.log(`Bob is ${age / 12} years old!`))
    .catch(error => console.error('Error:', error.message));
}

// Alternative with async/await
async function asyncPromiseExample(): Promise<void> {
  try {
    // This works fine
    const aliceAge = await promiseFindUserAgeByName(users, 'Alice');
    console.log(`Alice is ${aliceAge / 12} years old!`);
    
    // This throws, but we catch it
    const bobAge = await promiseFindUserAgeByName(emptyUsers, 'Bob');
    console.log(`Bob is ${bobAge / 12} years old!`);
  } catch (error) {
    console.error('Caught error:', error instanceof Error ? error.message : String(error));
  }
}

//==============================================================================
// APPROACH 3: OPTION PATTERN (SOME/NONE)
//==============================================================================

// Option pattern types
type Option<T> = Some<T> | None;

interface Some<T> {
  type: 'some';
  value: T;
}

interface None {
  type: 'none';
  error?: string;
}

const some = <T>(value: T): Some<T> => ({ type: 'some', value });
const none = (error?: string): None => ({ type: 'none', error });

/**
 * Option pattern approach - returns Some(value) or None
 * Advantages:
 * - Type signature (Option<number>) clearly indicates possibility of no value
 * - Forces handling of both cases at compile time
 * - No unexpected runtime crashes
 */
function optionFindUserAgeByName(users: User[], name: string): Option<number> {
  if (users.length === 0) {
    return none('There are no users!');
  }
  
  const user = users.find(u => u.name === name);
  
  if (!user) {
    return none(`User "${name}" not found!`);
  } 
  
  return some(user.ageInMonths);
}

// Example usage with Option pattern
function optionExample(): void {
  // This works fine
  const aliceResult = optionFindUserAgeByName(users, 'Alice');
  
  if (aliceResult.type === 'some') {
    console.log(`Alice is ${aliceResult.value / 12} years old!`);
  } else {
    console.error('Error:', aliceResult.error);
  }

  // This returns None, but our code handles it gracefully
  const bobResult = optionFindUserAgeByName(emptyUsers, 'Bob');
  
  if (bobResult.type === 'some') {
    console.log(`Bob is ${bobResult.value / 12} years old!`);
  } else {
    console.error('Error:', bobResult.error);
  }
}

//==============================================================================
// APPROACH 4: EITHER PATTERN (LEFT/RIGHT)
//==============================================================================

// Either pattern types
type Either<L, R> = Left<L> | Right<R>;

interface Left<L> {
  type: 'left';
  error: L;
}

interface Right<R> {
  type: 'right';
  value: R;
}

const left = <L, R = never>(error: L): Either<L, R> => ({ type: 'left', error });
const right = <R, L = never>(value: R): Either<L, R> => ({ type: 'right', value });

// Map function for Either
const map = <L, R, T>(either: Either<L, R>, fn: (value: R) => T): Either<L, T> => {
  if (either.type === 'right') {
    return right(fn(either.value));
  }
  return either as unknown as Either<L, T>;
};

/**
 * Either pattern approach - returns Right(value) or Left(error)
 * Advantages:
 * - Type signature (Either<string, number>) clearly indicates both success and error types
 * - Supports functional operations like map, flatMap
 * - Forces handling of both cases at compile time
 */
function eitherFindUserAgeByName(users: User[], name: string): Either<string, number> {
  if (users.length === 0) {
    return left('There are no users!');
  }
  
  const user = users.find(u => u.name === name);
  
  if (!user) {
    return left(`User "${name}" not found!`);
  } 
  
  return right(user.ageInMonths);
}

// Example usage with Either pattern
function eitherExample(): void {
  // Transform age to years using map
  const getAgeInYears = (result: Either<string, number>): Either<string, number> => {
    return map(result, age => age / 12);
  };

  // This works fine
  const aliceResult = getAgeInYears(eitherFindUserAgeByName(users, 'Alice'));
  
  if (aliceResult.type === 'right') {
    console.log(`Alice is ${aliceResult.value} years old!`);
  } else {
    console.error('Error:', aliceResult.error);
  }

  // This returns Left, but our code handles it gracefully
  const bobResult = getAgeInYears(eitherFindUserAgeByName(emptyUsers, 'Bob'));
  
  if (bobResult.type === 'right') {
    console.log(`Bob is ${bobResult.value} years old!`);
  } else {
    console.error('Error:', bobResult.error);
  }
}

//==============================================================================
// APPROACH 5: RESULT PATTERN (SUCCESS/FAILURE)
//==============================================================================

// Result pattern types
type Result<T, E = string> = Success<T> | Failure<E>;

interface Success<T> {
  success: true;
  value: T;
}

interface Failure<E> {
  success: false;
  error: E;
}

const success = <T>(value: T): Success<T> => ({ success: true, value });
const failure = <E>(error: E): Failure<E> => ({ success: false, error });

/**
 * Result pattern approach - returns Success(value) or Failure(error)
 * Advantages:
 * - Simple and intuitive boolean 'success' flag
 * - Type signature (Result<number, string>) clearly indicates both success and error types
 * - Very easy to check success/failure
 */
function resultFindUserAgeByName(users: User[], name: string): Result<number, string> {
  if (users.length === 0) {
    return failure('There are no users!');
  }
  
  const user = users.find(u => u.name === name);
  
  if (!user) {
    return failure(`User "${name}" not found!`);
  } 
  
  return success(user.ageInMonths);
}

// Example usage with Result pattern
function resultExample(): void {
  // This works fine
  const aliceResult = resultFindUserAgeByName(users, 'Alice');
  
  if (aliceResult.success) {
    console.log(`Alice is ${aliceResult.value / 12} years old!`);
  } else {
    console.error('Error:', aliceResult.error);
  }

  // This returns Failure, but our code handles it gracefully
  const bobResult = resultFindUserAgeByName(emptyUsers, 'Bob');
  
  console.log(
    bobResult.success
      ? `Bob is ${bobResult.value / 12} years old!`
      : `Error finding Bob's age: ${bobResult.error}`
  );
}

//==============================================================================
// COMPARISON RUNNER
//==============================================================================

/**
 * Run all examples to show the differences
 */
export const runComparison = (): void => {
  console.log('======== ORIGINAL APPROACH ========');
  originalExample();
  
  console.log('\n======== PROMISE APPROACH ========');
  promiseExample();
  
  console.log('\n======== ASYNC/AWAIT PROMISE APPROACH ========');
  asyncPromiseExample().catch(console.error);
  
  console.log('\n======== OPTION PATTERN ========');
  optionExample();
  
  console.log('\n======== EITHER PATTERN ========');
  eitherExample();
  
  console.log('\n======== RESULT PATTERN ========');
  resultExample();
};

// Uncomment to run the comparison
// runComparison(); 