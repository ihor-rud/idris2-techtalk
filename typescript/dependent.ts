type DependentRecord =
  | {
      isError: true;
      value: number;
    }
  | {
      isError: false;
      value: string;
    };

function someFunction(record: DependentRecord) {
  const value = record.value;
  if (record.isError) {
    const value = record.value;
  } else {
    const value = record.value;
  }
}
