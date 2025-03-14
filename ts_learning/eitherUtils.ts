/**
 * The Either type represents values with two possibilities: either Left (error) or Right (success)
 */
export type Either<L, R> = Left<L> | Right<R>;

/**
 * Left represents a failure value in the Either type
 */
export interface Left<L> {
  type: 'left';
  error: L;
}

/**
 * Right represents a success value in the Either type
 */
export interface Right<R> {
  type: 'right';
  value: R;
}

/**
 * Creates a Left instance containing an error
 */
export const left = <L, R = never>(error: L): Either<L, R> => ({
  type: 'left',
  error,
});

/**
 * Creates a Right instance containing a success value
 */
export const right = <R, L = never>(value: R): Either<L, R> => ({
  type: 'right',
  value,
});

/**
 * Maps a function over the right side of an Either
 */
export const map = <L, R, T>(
  either: Either<L, R>,
  fn: (value: R) => T
): Either<L, T> => {
  if (either.type === 'right') {
    return right(fn(either.value));
  }
  return either as unknown as Either<L, T>;
};

/**
 * Utility to safely apply operations that might fail
 */
export const tryCatch = <R, L = Error>(fn: () => R): Either<L, R> => {
  try {
    return right(fn());
  } catch (error) {
    return left(error as L);
  }
};

/**
 * User interface defining the structure of a user object
 */
export interface User {
  ageInMonths: number;
  name: string;
}

/**
 * Error types for user operations
 */
export enum UserErrorType {
  EMPTY_USERS_ARRAY = 'EMPTY_USERS_ARRAY',
  USER_NOT_FOUND = 'USER_NOT_FOUND',
}

/**
 * Error structure for user operations
 */
export interface UserError {
  type: UserErrorType;
  message: string;
}

/**
 * Finds a user's age by name, returning an Either
 * @param users - Array of User objects
 * @param name - Name to search for
 * @returns Either<UserError, number> - Right(age) if found, Left(error) if not found
 */
export const findUserAgeByName = (
  users: User[],
  name: string
): Either<UserError, number> => {
  if (users.length === 0) {
    return left({
      type: UserErrorType.EMPTY_USERS_ARRAY,
      message: 'There are no users!'
    });
  }

  const user = users.find(u => u.name === name);
  
  if (!user) {
    return left({
      type: UserErrorType.USER_NOT_FOUND,
      message: `User "${name}" not found!`
    });
  }
  
  return right(user.ageInMonths);
};

/**
 * Example of working with the Either type
 */
export const exampleUsage = (): void => {
  const users = [
    { ageInMonths: 36, name: 'Alice' },
    { ageInMonths: 48, name: 'Bob' }
  ];
  
  // Safe way to get the age in years
  const getAgeInYears = (result: Either<UserError, number>): Either<UserError, number> => {
    return map(result, age => age / 12);
  };
  
  // Example with a successful lookup
  const aliceAgeResult = findUserAgeByName(users, 'Alice');
  const aliceAgeInYears = getAgeInYears(aliceAgeResult);
  
  if (aliceAgeInYears.type === 'right') {
    console.log(`Alice is ${aliceAgeInYears.value} years old!`);
  } else {
    console.log(`Error: ${aliceAgeInYears.error.message}`);
  }
  
  // Example with a failed lookup
  const charlieAgeResult = findUserAgeByName(users, 'Charlie');
  const charlieAgeInYears = getAgeInYears(charlieAgeResult);
  
  if (charlieAgeInYears.type === 'right') {
    console.log(`Charlie is ${charlieAgeInYears.value} years old!`);
  } else {
    // We can also check the specific error type
    if (charlieAgeInYears.error.type === UserErrorType.USER_NOT_FOUND) {
      console.log(`Could not find Charlie in the user database`);
    } else {
      console.log(`Error: ${charlieAgeInYears.error.message}`);
    }
  }
  
  // Example with empty users array
  const emptyResult = findUserAgeByName([], 'Dave');
  
  // Pattern matching on Either
  switch (emptyResult.type) {
    case 'right':
      console.log(`Dave is ${emptyResult.value / 12} years old!`);
      break;
    case 'left':
      console.log(`Error lookup up Dave: ${emptyResult.error.message}`);
      break;
  }
}; 