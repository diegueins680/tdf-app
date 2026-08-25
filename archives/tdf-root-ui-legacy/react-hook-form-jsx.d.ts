import type { JSX } from 'react';
import type { ControllerProps, FieldPath, FieldValues } from 'react-hook-form';

declare module 'react-hook-form' {
  export function Controller<
    TFieldValues extends FieldValues = FieldValues,
    TName extends FieldPath<TFieldValues> = FieldPath<TFieldValues>,
    TTransformedValues = TFieldValues
  >(
    props: ControllerProps<TFieldValues, TName, TTransformedValues>
  ): JSX.Element;
}
