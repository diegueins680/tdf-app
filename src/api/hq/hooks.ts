import { useMemo } from 'react';
import {
  useMutation,
  useQuery,
  useQueryClient,
  type UseMutationOptions,
  type QueryKey,
} from '@tanstack/react-query';
import { hqClient, unwrap, unwrapRequired, unwrapVoid } from './client';
import type { components, paths } from '../generated/lessons-and-receipts';

type IdParam = components['parameters']['Id'];

type Teacher = components['schemas']['Teacher'];
type TeacherInput = components['schemas']['TeacherInput'];

type Student = components['schemas']['Student'];
type StudentInput = components['schemas']['StudentInput'];

type LessonPackage = components['schemas']['LessonPackage'];
type LessonPackageInput = components['schemas']['LessonPackageInput'];

type Enrollment = components['schemas']['Enrollment'];
type EnrollmentInput = components['schemas']['EnrollmentInput'];
type EnrollmentUpdate = components['schemas']['EnrollmentUpdate'];

type Lesson = components['schemas']['Lesson'];
type LessonInput = components['schemas']['LessonInput'];
type LessonUpdate = components['schemas']['LessonUpdate'];

type LessonMaterial = components['schemas']['LessonMaterial'];
type LessonMaterialInput = components['schemas']['LessonMaterialInput'];

type Payment = components['schemas']['Payment'];
type PaymentInput = components['schemas']['PaymentInput'];
type PaymentUpdate = components['schemas']['PaymentUpdate'];

type Receipt = components['schemas']['Receipt'];
type ReceiptInput = components['schemas']['ReceiptInput'];

type LessonsQuery = paths['/api/lessons']['get'] extends { parameters: { query?: infer Q } } ? Q : Record<string, never>;
type PaymentsQuery = paths['/api/payments']['get'] extends { parameters: { query?: infer Q } } ? Q : Record<string, never>;
type ReceiptsQuery = paths['/api/receipts']['get'] extends { parameters: { query?: infer Q } } ? Q : Record<string, never>;

const hqKeys = {
  teachers: ['hq', 'teachers'] as const,
  teacher: (id: string) => ['hq', 'teachers', id] as const,
  students: ['hq', 'students'] as const,
  student: (id: string) => ['hq', 'students', id] as const,
  packages: ['hq', 'packages'] as const,
  package: (id: string) => ['hq', 'packages', id] as const,
  enrollments: ['hq', 'enrollments'] as const,
  enrollment: (id: string) => ['hq', 'enrollments', id] as const,
  lessonsRoot: ['hq', 'lessons'] as const,
  lessons: (filters?: LessonsQuery) => ['hq', 'lessons', filters ?? {}] as const,
  lesson: (id: string) => ['hq', 'lessons', id] as const,
  lessonMaterials: (lessonId: string) => ['hq', 'lessons', lessonId, 'materials'] as const,
  paymentsRoot: ['hq', 'payments'] as const,
  payments: (filters?: PaymentsQuery) => ['hq', 'payments', filters ?? {}] as const,
  payment: (id: string) => ['hq', 'payments', id] as const,
  receiptsRoot: ['hq', 'receipts'] as const,
  receipts: (filters?: ReceiptsQuery) => ['hq', 'receipts', filters ?? {}] as const,
  receipt: (id: string) => ['hq', 'receipts', id] as const,
  receiptPdf: (id: string) => ['hq', 'receipts', id, 'pdf'] as const,
};

function withInvalidate<TData, TVariables>(
  queryClient: ReturnType<typeof useQueryClient>,
  keys: QueryKey | QueryKey[],
  mutation: UseMutationOptions<TData, Error, TVariables, unknown> | undefined,
) {
  const { onSuccess: userOnSuccess, ...rest } = mutation ?? {};
  const targetKeys = Array.isArray(keys) ? keys : [keys];
  return {
    ...rest,
    onSuccess: (data, variables, onMutateResult, context) => {
      for (const key of targetKeys) {
        queryClient.invalidateQueries({ queryKey: key, exact: false });
      }
      userOnSuccess?.(data, variables, onMutateResult, context);
    },
  } satisfies UseMutationOptions<TData, Error, TVariables, unknown>;
}

export function useTeachersQuery() {
  return useQuery({
    queryKey: hqKeys.teachers,
    queryFn: () => unwrap(hqClient.GET<Teacher[]>('/api/teachers')),
  });
}

export function useTeacherQuery(id: IdParam | null | undefined) {
  return useQuery({
    queryKey: id ? hqKeys.teacher(id) : ['hq', 'teachers', 'detail', 'empty'],
    queryFn: () => unwrap(hqClient.GET<Teacher>('/api/teachers/{id}', { params: { path: { id: id as IdParam } } })),
    enabled: Boolean(id),
  });
}

export function useCreateTeacherMutation(options?: UseMutationOptions<Teacher, Error, TeacherInput>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (input: TeacherInput) => unwrapRequired(hqClient.POST<Teacher>('/api/teachers', { body: input })),
    ...withInvalidate(queryClient, hqKeys.teachers, options),
  });
}

export function useUpdateTeacherMutation(id: IdParam, options?: UseMutationOptions<Teacher, Error, TeacherInput>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (input: TeacherInput) =>
      unwrapRequired(hqClient.PATCH<Teacher>('/api/teachers/{id}', { params: { path: { id } }, body: input })),
    ...withInvalidate(queryClient, [hqKeys.teachers, hqKeys.teacher(id)], options),
  });
}

export function useDeleteTeacherMutation(id: IdParam, options?: UseMutationOptions<void, Error, void>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: () => unwrapVoid(hqClient.DELETE('/api/teachers/{id}', { params: { path: { id } } })),
    ...withInvalidate(queryClient, [hqKeys.teachers, hqKeys.teacher(id)], options),
  });
}

export function useStudentsQuery() {
  return useQuery({
    queryKey: hqKeys.students,
    queryFn: () => unwrap(hqClient.GET<Student[]>('/api/students')),
  });
}

export function useStudentQuery(id: IdParam | null | undefined) {
  return useQuery({
    queryKey: id ? hqKeys.student(id) : ['hq', 'students', 'detail', 'empty'],
    queryFn: () => unwrap(hqClient.GET<Student>('/api/students/{id}', { params: { path: { id: id as IdParam } } })),
    enabled: Boolean(id),
  });
}

export function useCreateStudentMutation(options?: UseMutationOptions<Student, Error, StudentInput>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (input: StudentInput) => unwrapRequired(hqClient.POST<Student>('/api/students', { body: input })),
    ...withInvalidate(queryClient, hqKeys.students, options),
  });
}

export function useUpdateStudentMutation(id: IdParam, options?: UseMutationOptions<Student, Error, StudentInput>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (input: StudentInput) =>
      unwrapRequired(hqClient.PATCH<Student>('/api/students/{id}', { params: { path: { id } }, body: input })),
    ...withInvalidate(queryClient, [hqKeys.students, hqKeys.student(id)], options),
  });
}

export function useDeleteStudentMutation(id: IdParam, options?: UseMutationOptions<void, Error, void>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: () => unwrapVoid(hqClient.DELETE('/api/students/{id}', { params: { path: { id } } })),
    ...withInvalidate(queryClient, [hqKeys.students, hqKeys.student(id)], options),
  });
}

export function useLessonPackagesQuery() {
  return useQuery({
    queryKey: hqKeys.packages,
    queryFn: () => unwrap(hqClient.GET<LessonPackage[]>('/api/packages')),
  });
}

export function useLessonPackageQuery(id: IdParam | null | undefined) {
  return useQuery({
    queryKey: id ? hqKeys.package(id) : ['hq', 'packages', 'detail', 'empty'],
    queryFn: () => unwrap(hqClient.GET<LessonPackage>('/api/packages/{id}', { params: { path: { id: id as IdParam } } })),
    enabled: Boolean(id),
  });
}

export function useCreateLessonPackageMutation(options?: UseMutationOptions<LessonPackage, Error, LessonPackageInput>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (input: LessonPackageInput) => unwrapRequired(hqClient.POST<LessonPackage>('/api/packages', { body: input })),
    ...withInvalidate(queryClient, hqKeys.packages, options),
  });
}

export function useUpdateLessonPackageMutation(
  id: IdParam,
  options?: UseMutationOptions<LessonPackage, Error, LessonPackageInput>,
) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (input: LessonPackageInput) =>
      unwrapRequired(hqClient.PATCH<LessonPackage>('/api/packages/{id}', { params: { path: { id } }, body: input })),
    ...withInvalidate(queryClient, [hqKeys.packages, hqKeys.package(id)], options),
  });
}

export function useArchiveLessonPackageMutation(id: IdParam, options?: UseMutationOptions<void, Error, void>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: () => unwrapVoid(hqClient.DELETE('/api/packages/{id}', { params: { path: { id } } })),
    ...withInvalidate(queryClient, [hqKeys.packages, hqKeys.package(id)], options),
  });
}

export function useEnrollmentsQuery() {
  return useQuery({
    queryKey: hqKeys.enrollments,
    queryFn: () => unwrap(hqClient.GET<Enrollment[]>('/api/enrollments')),
  });
}

export function useEnrollmentQuery(id: IdParam | null | undefined) {
  return useQuery({
    queryKey: id ? hqKeys.enrollment(id) : ['hq', 'enrollments', 'detail', 'empty'],
    queryFn: () => unwrap(hqClient.GET<Enrollment>('/api/enrollments/{id}', { params: { path: { id: id as IdParam } } })),
    enabled: Boolean(id),
  });
}

export function useCreateEnrollmentMutation(options?: UseMutationOptions<Enrollment, Error, EnrollmentInput>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (input: EnrollmentInput) => unwrapRequired(hqClient.POST<Enrollment>('/api/enrollments', { body: input })),
    ...withInvalidate(queryClient, hqKeys.enrollments, options),
  });
}

export function useUpdateEnrollmentMutation(id: IdParam, options?: UseMutationOptions<Enrollment, Error, EnrollmentUpdate>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (input: EnrollmentUpdate) =>
      unwrapRequired(hqClient.PATCH<Enrollment>('/api/enrollments/{id}', { params: { path: { id } }, body: input })),
    ...withInvalidate(queryClient, [hqKeys.enrollments, hqKeys.enrollment(id)], options),
  });
}

export function useDeleteEnrollmentMutation(id: IdParam, options?: UseMutationOptions<void, Error, void>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: () => unwrapVoid(hqClient.DELETE('/api/enrollments/{id}', { params: { path: { id } } })),
    ...withInvalidate(queryClient, [hqKeys.enrollments, hqKeys.enrollment(id)], options),
  });
}

export function useLessonsQuery(filters?: LessonsQuery) {
  const sanitizedFilters = useMemo(() => filters ?? {}, [filters]);
  return useQuery({
    queryKey: hqKeys.lessons(sanitizedFilters),
    queryFn: () =>
      unwrap(
        hqClient.GET<Lesson[]>(
          '/api/lessons',
          {
            params: Object.keys(sanitizedFilters).length ? { query: sanitizedFilters } : undefined,
          },
        ),
      ),
  });
}

export function useLessonQuery(id: IdParam | null | undefined) {
  return useQuery({
    queryKey: id ? hqKeys.lesson(id) : ['hq', 'lessons', 'detail', 'empty'],
    queryFn: () => unwrap(hqClient.GET<Lesson>('/api/lessons/{id}', { params: { path: { id: id as IdParam } } })),
    enabled: Boolean(id),
  });
}

export function useCreateLessonMutation(options?: UseMutationOptions<Lesson, Error, LessonInput>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (input: LessonInput) => unwrapRequired(hqClient.POST<Lesson>('/api/lessons', { body: input })),
    ...withInvalidate(queryClient, [hqKeys.lessonsRoot, hqKeys.enrollments], options),
  });
}

export function useUpdateLessonMutation(id: IdParam, options?: UseMutationOptions<Lesson, Error, LessonUpdate>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (input: LessonUpdate) =>
      unwrapRequired(hqClient.PATCH<Lesson>('/api/lessons/{id}', { params: { path: { id } }, body: input })),
    ...withInvalidate(queryClient, [hqKeys.lessonsRoot, hqKeys.lesson(id)], options),
  });
}

export function useDeleteLessonMutation(id: IdParam, options?: UseMutationOptions<void, Error, void>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: () => unwrapVoid(hqClient.DELETE('/api/lessons/{id}', { params: { path: { id } } })),
    ...withInvalidate(queryClient, [hqKeys.lessonsRoot, hqKeys.lesson(id)], options),
  });
}

export function useLessonMaterialsQuery(lessonId: IdParam | null | undefined) {
  return useQuery({
    queryKey: lessonId ? hqKeys.lessonMaterials(lessonId) : ['hq', 'lessons', 'materials', 'empty'],
    queryFn: () =>
      unwrap(
        hqClient.GET<LessonMaterial[]>(
          '/api/lessons/{id}/materials',
          {
            params: { path: { id: lessonId as IdParam } },
          },
        ),
      ),
    enabled: Boolean(lessonId),
  });
}

export function useCreateLessonMaterialMutation(
  lessonId: IdParam,
  options?: UseMutationOptions<LessonMaterial, Error, LessonMaterialInput>,
) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (input: LessonMaterialInput) =>
      unwrapRequired(
        hqClient.POST<LessonMaterial>(
          '/api/lessons/{id}/materials',
          {
            params: { path: { id: lessonId } },
            body: input,
          },
        ),
      ),
    ...withInvalidate(queryClient, hqKeys.lessonMaterials(lessonId), options),
  });
}

export function usePaymentsQuery(filters?: PaymentsQuery) {
  const sanitizedFilters = useMemo(() => filters ?? {}, [filters]);
  return useQuery({
    queryKey: hqKeys.payments(sanitizedFilters),
    queryFn: () =>
      unwrap(
        hqClient.GET<Payment[]>(
          '/api/payments',
          {
            params: Object.keys(sanitizedFilters).length ? { query: sanitizedFilters } : undefined,
          },
        ),
      ),
  });
}

export function usePaymentQuery(id: IdParam | null | undefined) {
  return useQuery({
    queryKey: id ? hqKeys.payment(id) : ['hq', 'payments', 'detail', 'empty'],
    queryFn: () => unwrap(hqClient.GET<Payment>('/api/payments/{id}', { params: { path: { id: id as IdParam } } })),
    enabled: Boolean(id),
  });
}

export function useCreatePaymentMutation(options?: UseMutationOptions<Payment, Error, PaymentInput>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (input: PaymentInput) => unwrapRequired(hqClient.POST<Payment>('/api/payments', { body: input })),
    ...withInvalidate(queryClient, [hqKeys.paymentsRoot, hqKeys.enrollments], options),
  });
}

export function useUpdatePaymentMutation(id: IdParam, options?: UseMutationOptions<Payment, Error, PaymentUpdate>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (input: PaymentUpdate) =>
      unwrapRequired(hqClient.PATCH<Payment>('/api/payments/{id}', { params: { path: { id } }, body: input })),
    ...withInvalidate(queryClient, [hqKeys.paymentsRoot, hqKeys.payment(id)], options),
  });
}

export function useDeletePaymentMutation(id: IdParam, options?: UseMutationOptions<void, Error, void>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: () => unwrapVoid(hqClient.DELETE('/api/payments/{id}', { params: { path: { id } } })),
    ...withInvalidate(queryClient, [hqKeys.paymentsRoot, hqKeys.payment(id)], options),
  });
}

export function useReceiptsQuery(filters?: ReceiptsQuery) {
  const sanitizedFilters = useMemo(() => filters ?? {}, [filters]);
  return useQuery({
    queryKey: hqKeys.receipts(sanitizedFilters),
    queryFn: () =>
      unwrap(
        hqClient.GET<Receipt[]>(
          '/api/receipts',
          {
            params: Object.keys(sanitizedFilters).length ? { query: sanitizedFilters } : undefined,
          },
        ),
      ),
  });
}

export function useReceiptQuery(id: IdParam | null | undefined) {
  return useQuery({
    queryKey: id ? hqKeys.receipt(id) : ['hq', 'receipts', 'detail', 'empty'],
    queryFn: () => unwrap(hqClient.GET<Receipt>('/api/receipts/{id}', { params: { path: { id: id as IdParam } } })),
    enabled: Boolean(id),
  });
}

export function useReceiptPdfQuery(id: IdParam | null | undefined) {
  return useQuery({
    queryKey: id ? hqKeys.receiptPdf(id) : ['hq', 'receipts', 'pdf', 'empty'],
    queryFn: () =>
      unwrapRequired(
        hqClient.GET<Blob>('/api/receipts/{id}/pdf', {
          params: { path: { id: id as IdParam } },
          parseAs: 'blob',
        }),
      ),
    enabled: Boolean(id),
  });
}

export function useCreateReceiptMutation(options?: UseMutationOptions<Receipt, Error, ReceiptInput>) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (input: ReceiptInput) => unwrapRequired(hqClient.POST<Receipt>('/api/receipts', { body: input })),
    ...withInvalidate(queryClient, [hqKeys.receiptsRoot, hqKeys.paymentsRoot, hqKeys.enrollments], options),
  });
}

export { hqKeys };
