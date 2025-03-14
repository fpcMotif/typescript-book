/**
 * Represents an optional value - either Some(value) or None
 */
export type Option<T> = Some<T> | None;

/**
 * Represents the presence of a value
 */
export interface Some<T> {
  type: 'some';
  value: T;
}

/**
 * Represents the absence of a value
 */
export interface None {
  type: 'none';
  error?: string;
}

/**
 * Creates a Some instance containing a value
 */
export const some = <T>(value: T): Some<T> => ({
  type: 'some',
  value,
});

/**
 * Creates a None instance, optionally with an error message
 */
export const none = (error?: string): None => ({
  type: 'none',
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
 * Finds a user's age by name, returning an Option
 * @param users - Array of User objects
 * @param name - Name to search for
 * @returns Option<number> - Some(age) if found, None if not found
 */
export const findUserAgeByName = (users: User[], name: string): Option<number> => {
  if (users.length === 0) {
    return none('There are no users!');
  }

  const user = users.find(u => u.name === name);
  
  if (!user) {
    return none(`User "${name}" not found!`);
  }
  
  return some(user.ageInMonths);
};

/**
 * Example of working with the Option type
 */
export const exampleUsage = (): void => {
  const users = [
    { ageInMonths: 36, name: 'Alice' },
    { ageInMonths: 48, name: 'Bob' }
  ];
  
  // Safe way to use the result
  const aliceAgeResult = findUserAgeByName(users, 'Alice');
  
  // We must check the type before using the value
  if (aliceAgeResult.type === 'some') {
    const ageInYears = aliceAgeResult.value / 12;
    console.log(`Alice is ${ageInYears} years old!`);
  } else {
    console.log(`Error: ${aliceAgeResult.error}`);
  }
  
  // Another example with a missing user
  const charlieAgeResult = findUserAgeByName(users, 'Charlie');
  
  // Pattern matching (simplified in TypeScript)
  switch (charlieAgeResult.type) {
    case 'some':
      const ageInYears = charlieAgeResult.value / 12;
      console.log(`Charlie is ${ageInYears} years old!`);
      break;
    case 'none':
      console.log(`Error: ${charlieAgeResult.error}`);
      break;
  }
  
  // You can also use a helper function to transform the result
  const getAgeInYears = (result: Option<number>): string => {
    return result.type === 'some'
      ? `${result.value / 12} years old`
      : `Unknown age: ${result.error}`;
  };
  
  console.log(`Bob is ${getAgeInYears(findUserAgeByName(users, 'Bob'))}`);
  console.log(`Dave is ${getAgeInYears(findUserAgeByName(users, 'Dave'))}`);
}; 