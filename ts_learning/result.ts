/**
 * A lightweight Result type for handling operations that might fail
 */
export type Result<T, E = Error> = Success<T> | Failure<E>;

/**
 * Represents a successful operation with a value
 */
export interface Success<T> {
  success: true;
  value: T;
}

/**
 * Represents a failed operation with an error
 */
export interface Failure<E> {
  success: false;
  error: E;
}

/**
 * Creates a success result
 */
export const success = <T>(value: T): Success<T> => ({
  success: true,
  value,
});

/**
 * Creates a failure result
 */
export const failure = <E>(error: E): Failure<E> => ({
  success: false,
  error,
});

/**
 * User interface defining the structure of a user object
 */
export interface User {
  ageInMonths: number;
  name: string;
}

/**
 * Finds a user's age by name using the Result pattern
 */
export const findUserAgeByName = (
  users: User[],
  name: string
): Result<number, string> => {
  if (users.length === 0) {
    return failure('There are no users!');
  }

  const user = users.find(u => u.name === name);
  
  if (!user) {
    return failure(`User "${name}" not found!`);
  }
  
  return success(user.ageInMonths);
};

/**
 * Example of using the Result pattern
 */
export const exampleUsage = (): void => {
  const users = [
    { ageInMonths: 36, name: 'Alice' },
    { ageInMonths: 48, name: 'Bob' }
  ];
  
  // Safely find Alice's age
  const aliceResult = findUserAgeByName(users, 'Alice');
  
  // The TypeScript compiler forces us to check success before using the value
  if (aliceResult.success) {
    console.log(`Alice is ${aliceResult.value / 12} years old`);
  } else {
    console.log(`Error: ${aliceResult.error}`);
  }
  
  // Safely find Charlie's age (who doesn't exist)
  const charlieResult = findUserAgeByName(users, 'Charlie');
  
  // Using a more concise pattern to handle the result
  console.log(
    charlieResult.success
      ? `Charlie is ${charlieResult.value / 12} years old`
      : `Error: ${charlieResult.error}`
  );
  
  // We can also chain operations with results
  const getAgeInYears = (result: Result<number, string>): Result<string, string> => {
    if (result.success) {
      const ageInYears = result.value / 12;
      return success(`${ageInYears} years old`);
    }
    return failure(result.error);
  };
  
  const bobAgeResult = getAgeInYears(findUserAgeByName(users, 'Bob'));
  
  if (bobAgeResult.success) {
    console.log(`Bob is ${bobAgeResult.value}`);
  } else {
    console.log(`Could not find Bob's age: ${bobAgeResult.error}`);
  }
}; 