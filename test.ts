// Learning TypeScript
const firstName = "Georgia";
const nameLength = firstName.length();

/*
The code is written in normal JavaScript syntax—I haven't introduced TypeScriptspecific syntax yet. If you were to run the TypeScript type checker on this code, it
would use its knowledge that the length property of a string is a number—not a
function—to give you the complaint shown in the comment.
*/


// Previously: sayMyName(firstName, lastName) { ...
function sayMyName(fullName) {
console.log(`You acting kind of shady, ain't callin' me ${fullName}`);
}
sayMyName("Beyoncé", "Knowles");
// ~~~~~~~~~
// Expected 1 argument, but got 2.


interface Material {
    name: string;
    type: string;
}

interface Painter {
finish(): boolean;
ownMaterials: Material[];
paint(painting: string, materials: Material[]): boolean;
}

function paintPainting(painter: Painter, painting: string): boolean { /* ... */ }


Painter.paint("The Starry Night", ["oil", "canvas"]);


// Inferred type: string
let bestSong = Math.random() > 0.5
? "Chain of Fools"
: "Respect";


let singer = {
    name: "Beyoncé",
    age: 39,
    bestSong: bestSong,
    sayMyName: sayMyName
}



//let rocker; // Type: any
let rocker: string | number; // Type explicitly set
rocker = "Joan Jett"; // Type: string
rocker.toUpperCase(); // Ok

rocker = 19.58; // Type: number
rocker.toPrecision(1); // Ok

//rocker.toUpperCase();
// Check if rocker is a string before calling toUpperCase
if (typeof rocker === "string") {
  rocker.toUpperCase(); // This line is now safe
}








let cher = {
firstName: "Cherilyn",
lastName: "Sarkisian",
};
cher.middleName = "Sarkisian";





const a = null + 7;
const b = [] + 12;
alert('Hello', 'TypeScript');



// Literal Types


const philosopher = "Hypatia";
const year = philosopher.birth;





let mathematician: "string" | "number";
mathematician = "Hypatia";
mathematician = 3.14;


const mathematicians: string[] = ["Hypatia", "Pythagoras", "Euclid"];


const mathematician1 = "Mark Gromov";


// TypeScript reporting a let variable as being generally its primitive type
/*
think of each primitive type as a union of every possible matching literal
value. In other words, a primitive type is the set of all possible literal values of that
type.
*/
let mathematician2 = "Grigori Perelman";





let lifespan: number | "ongoing" | "uncertain";
lifespan = 89; // Ok
lifespan = "ongoing"; // Ok
lifespan = true;







let specificallyAda: "Ada";
specificallyAda = "Ada"; // Ok
specificallyAda = "Byron";
// Error: Type '"Byron"' is not assignable to type '"Ada"'.
let someString = ""; // Type: string
specificallyAda = someString;


let nameMaybe = Math.random() > 0.5
? "Tony Hoare"
: undefined;
nameMaybe.toLowerCase();




let nameMaybe = Math.random() > 0.5
? "Tony Hoare"
: undefined;
nameMaybe.toLowerCase();



let geneticist = Math.random() > 0.5
? "Barbara McClintock"
: undefined;
if (geneticist) {
geneticist.toUpperCase(); // Ok: string
}
geneticist.toUpperCase();





let biologist = Math.random() > 0.5 && "Rachel Carson";
if (biologist) {
biologist; // Type: string
} else {
biologist; // Type: false | string
}



let mathematiciann: string;
mathematiciann?.length;
mathematiciann = "Mark Goldberg";
mathematiciann.length; // Ok



let mathematicianm: string | undefined;
mathematicianm?.length; // Ok
mathematicianm = "Mark Goldberg";
mathematicianm.length; // Ok


