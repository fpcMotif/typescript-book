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
 * Custom error class for user-related errors
 */
export class UserError extends Error {
  type: UserErrorType;

  constructor(message: string, type: UserErrorType) {
    super(message);
    this.type = type;
    this.name = 'UserError';
  }
}

/**
 * Finds a user's age in months by their name
 * @param users - Array of User objects to search through
 * @param name - Name of the user to find
 * @returns The user's age in months
 * @throws UserError if users array is empty or user is not found
 */
export const findUserAgeByName = (users: User[], name: string): number => {
  // Early return if users array is empty
  if (users.length === 0) {
    throw new UserError(
      'Cannot find user age: There are no users!',
      UserErrorType.EMPTY_USERS_ARRAY
    );
  }

  // Find the user by name
  const user = users.find((u) => u.name === name);

  // Early return if user not found
  if (!user) {
    throw new UserError(
      `Cannot find user age: User "${name}" not found!`,
      UserErrorType.USER_NOT_FOUND
    );
  }

  // Return the age if user found
  return user.ageInMonths;
};

/**
 * Safely attempts to find a user's age by name without throwing errors
 * @param users - Array of User objects to search through
 * @param name - Name of the user to find
 * @returns An object containing the age or error information
 */
export const safelyFindUserAgeByName = (
  users: User[],
  name: string
): { success: boolean; age?: number; error?: string; errorType?: UserErrorType } => {
  try {
    const age = findUserAgeByName(users, name);
    return { success: true, age };
  } catch (error) {
    if (error instanceof UserError) {
      return {
        success: false,
        error: error.message,
        errorType: error.type,
      };
    }
    // Handle unexpected errors
    return {
      success: false,
      error: 'An unexpected error occurred',
    };
  }
}; 