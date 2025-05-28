type FullName = {
  firstName: string;
  lastName: string;
};

function initials({ firstName, lastName }: FullName): [string, string] {
  return [firstName.slice(0, 1), lastName.slice(0, 1)];
}
