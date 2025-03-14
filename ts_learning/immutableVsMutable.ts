/**
 * IMMUTABLE VS MUTABLE APPROACHES
 * 
 * This file demonstrates the key differences between mutable and immutable approaches
 * to data manipulation in TypeScript.
 */

// ======== MUTABLE APPROACH ========

/**
 * A mutable list implementation that directly modifies its internal state
 */
class MutableList<T> {
  // The list can be modified directly
  private items: T[];

  constructor(initialItems: T[] = []) {
    this.items = [...initialItems];
  }

  /**
   * Add an item by directly modifying the internal array
   */
  add(item: T): void {
    this.items.push(item);
  }

  /**
   * Remove an item by directly modifying the internal array
   */
  remove(item: T): void {
    const index = this.items.findIndex(i => i === item);
    if (index !== -1) {
      this.items.splice(index, 1);
    }
  }

  /**
   * Get an item - returns direct reference to the internal item
   */
  get(index: number): T | undefined {
    return this.items[index];
  }

  /**
   * Find an item - returns direct reference to the internal item
   */
  find(predicate: (item: T) => boolean): T | undefined {
    return this.items.find(predicate);
  }

  /**
   * Get all items as an array - returns a reference to the original array
   */
  toArray(): T[] {
    return this.items;
  }
}

// ======== IMMUTABLE APPROACH ========

/**
 * An immutable list implementation that never modifies its internal state
 */
class ImmutableList<T> {
  private readonly items: ReadonlyArray<T>;

  constructor(initialItems: T[] = []) {
    this.items = [...initialItems];
  }

  /**
   * Deep clone an item to ensure immutability
   */
  private deepClone(item: T): T {
    return JSON.parse(JSON.stringify(item));
  }

  /**
   * Add an item by creating a NEW list
   */
  add(item: T): ImmutableList<T> {
    const newArray = [...this.items, this.deepClone(item)];
    return new ImmutableList<T>(newArray);
  }

  /**
   * Remove an item by creating a NEW list
   */
  remove(item: T): ImmutableList<T> {
    const newArray = this.items
      .filter(i => i !== item)
      .map(i => this.deepClone(i));
    return new ImmutableList<T>(newArray);
  }

  /**
   * Get an item - returns a clone to prevent modification
   */
  get(index: number): T | undefined {
    const item = this.items[index];
    return item ? this.deepClone(item) : undefined;
  }

  /**
   * Find an item - returns a clone to prevent modification
   */
  find(predicate: (item: T) => boolean): T | undefined {
    const item = this.items.find(predicate);
    return item ? this.deepClone(item) : undefined;
  }

  /**
   * Get all items as an array - returns clones to prevent modification
   */
  toArray(): T[] {
    return this.items.map(item => this.deepClone(item));
  }
}

// ======== DEMONSTRATION ========

// Define a task type for our examples
interface Task {
  id: number;
  title: string;
  completed: boolean;
  tags: string[];
}

// Initial tasks
const initialTasks: Task[] = [
  { id: 1, title: "Learn TypeScript", completed: false, tags: ["programming", "study"] },
  { id: 2, title: "Buy groceries", completed: true, tags: ["shopping", "errands"] }
];

/**
 * Demonstrate the dangers of mutable data structures
 */
function demonstrateMutableProblem(): void {
  console.log("MUTABLE LIST DEMONSTRATION");
  console.log("===========================");

  // Create a mutable list
  const taskList = new MutableList<Task>(initialTasks);
  
  // Get a reference to the first task
  const task = taskList.find(t => t.id === 1);
  console.log("Original task:", JSON.stringify(task));
  
  // Modify the task through the reference
  if (task) {
    task.completed = true;
    task.tags.push("important");
    console.log("Modified task via reference:", JSON.stringify(task));
  }
  
  // The original list is now modified!
  const sameTask = taskList.find(t => t.id === 1);
  console.log("Task in the list is now modified:", JSON.stringify(sameTask));
  
  console.log("\nWhen you change an object you got from a mutable list, it changes the original object in the list too!");
  console.log("This can lead to unexpected bugs and side effects.\n");
}

/**
 * Demonstrate how immutable data structures prevent unexpected changes
 */
function demonstrateImmutableSolution(): void {
  console.log("IMMUTABLE LIST DEMONSTRATION");
  console.log("============================");

  // Create an immutable list
  const taskList = new ImmutableList<Task>(initialTasks);
  
  // Get a copy of the first task
  const task = taskList.find(t => t.id === 1);
  console.log("Original task:", JSON.stringify(task));
  
  // Modify the task (this is just our local copy)
  if (task) {
    task.completed = true;
    task.tags.push("important");
    console.log("Modified our copy of the task:", JSON.stringify(task));
  }
  
  // The original list is still unchanged!
  const originalTask = taskList.find(t => t.id === 1);
  console.log("Task in the immutable list is unchanged:", JSON.stringify(originalTask));
  
  // Proper way to update in an immutable world: create a new list
  const updatedTask = { ...originalTask as Task, completed: true, tags: [...(originalTask as Task).tags, "important"] };
  const newTaskList = taskList.add(updatedTask).remove(originalTask as Task);
  
  console.log("\nWhen you change a copy from an immutable list, the original stays the same!");
  console.log("You must explicitly create a new list with your changes.\n");
}

/**
 * Run both demonstrations to see the difference
 */
export function compareMutableVsImmutable(): void {
  demonstrateMutableProblem();
  console.log("\n");
  demonstrateImmutableSolution();
}

// Uncomment to run the comparison
// compareMutableVsImmutable(); 