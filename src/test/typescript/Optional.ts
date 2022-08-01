function buildName(firstName: string, lastName?: string) {
    return firstName + lastName;
}

function buildName2(firstName: string, lastName = "DIO") {
    return firstName + lastName;
}