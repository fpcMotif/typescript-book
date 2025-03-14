/**
 * A simplified immutable list implementation to demonstrate immutability concepts
 */
class ImmutableList<T> {
  // ReadonlyArray means we can't modify this array directly
  private readonly items: ReadonlyArray<T>;

  /**
   * Create a new immutable list
   */
  constructor(initialItems: T[] = []) {
    // Store a copy of the initial items
    this.items = [...initialItems];
  }

  /**
   * Create a deep clone of an item to ensure immutability
   */
  private deepClone(item: T): T {
    return JSON.parse(JSON.stringify(item));
  }

  /**
   * Add a new item to the list
   * Returns a NEW list rather than modifying the existing one
   */
  add(newItem: T): ImmutableList<T> {
    // Create a new array with all existing items + the new item
    const newArray = [...this.items, this.deepClone(newItem)];
    
    // Return a completely new ImmutableList instance
    return new ImmutableList<T>(newArray);
  }

  /**
   * Remove an item from the list
   * Returns a NEW list rather than modifying the existing one
   */
  remove(itemToRemove: T, areEqual: (a: T, b: T) => boolean = (a, b) => a === b): ImmutableList<T> {
    // Filter out the item to remove and clone each remaining item
    const newArray = this.items
      .filter(item => !areEqual(item, itemToRemove))
      .map(item => this.deepClone(item));
    
    // Return a completely new ImmutableList instance
    return new ImmutableList<T>(newArray);
  }

  /**
   * Get an item at a specific index
   * Returns a clone of the item to prevent modification of internal state
   */
  get(index: number): T | undefined {
    const item = this.items[index];
    // Return a clone to prevent modifying the internal item
    return item ? this.deepClone(item) : undefined;
  }

  /**
   * Find an item using a predicate function
   * Returns a clone of the item to prevent modification of internal state
   */
  find(predicate: (item: T) => boolean): T | undefined {
    const item = this.items.find(predicate);
    // Return a clone to prevent modifying the internal item
    return item ? this.deepClone(item) : undefined;
  }

  /**
   * Get all items as an array
   * Returns a deep clone of all items
   */
  toArray(): T[] {
    return this.items.map(item => this.deepClone(item));
  }
}

/**
 * Example usage of ImmutableList
 */
interface Hero {
  name: string;
  powers: string[];
}

// Initial data
const initialHeroes: Hero[] = [
  {
    name: "Spiderman",
    powers: [
      "wall-crawling",
      "enhanced strength",
      "enhanced speed",
      "spider-sense"
    ]
  },
  {
    name: "Superman",
    powers: [
      "flight",
      "superhuman strength",
      "x-ray vision",
      "super-speed"
    ]
  }
];

// Hero to add later
const hulk: Hero = {
  name: "Hulk",
  powers: [
    "superhuman strength",
    "superhuman speed",
    "superhuman stamina",
    "superhuman durability"
  ]
};

// Demo function to show how immutability works
function demoImmutability(): void {
  console.log("IMMUTABILITY DEMONSTRATION");
  console.log("==========================");

  // Create an initial list
  const list1 = new ImmutableList<Hero>(initialHeroes);
  console.log("Original list created");
  
  // Add Hulk to create a new list
  const list2 = list1.add(hulk);
  console.log("Created a new list by adding Hulk");
  
  // Check if Hulk exists in both lists
  const hulkInList1 = list1.find(h => h.name === "Hulk");
  console.log("Is Hulk in original list?", hulkInList1 ? "Yes" : "No");
  
  const hulkInList2 = list2.find(h => h.name === "Hulk");
  console.log("Is Hulk in new list?", hulkInList2 ? "Yes" : "No");
  
  // Prove they are different objects
  console.log("Are the two lists the same object?", list1 === list2 ? "Yes" : "No");
  
  // Try to modify a hero in list2 and show it doesn't affect list1
  const spidermanFromList2 = list2.find(h => h.name === "Spiderman");
  if (spidermanFromList2) {
    // If we could modify the internal data, this would affect both lists
    // But because we're returning clones, it doesn't
    spidermanFromList2.powers.push("spider-webs");
    console.log("Added 'spider-webs' power to Spiderman from list2");
  }
  
  // Show that original Spiderman in list1 is unchanged
  const spidermanFromList1 = list1.find(h => h.name === "Spiderman");
  console.log("Spiderman powers in original list:", spidermanFromList1?.powers);
  
  // Show the modified Spiderman is still unmodified in list2 too
  // (because find() returns a clone)
  const spidermanFromList2Again = list2.find(h => h.name === "Spiderman");
  console.log("Spiderman powers in second list:", spidermanFromList2Again?.powers);
}

// Run the demo
// demoImmutability(); 