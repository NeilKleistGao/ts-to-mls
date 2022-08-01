function buildName(firstName: string, lastName?: string) {
    return firstName + lastName;
}

function buildName2(firstName: string, lastName = "DIO") {
    return firstName + lastName;
}

function buildName3(firstName: string, ...lastName) {
    console.log(lastName)
    return firstName;
}

interface SquareConfig {
    color?: string;
    width?: number;
}