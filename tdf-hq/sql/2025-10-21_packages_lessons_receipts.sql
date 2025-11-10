-- TDF HQ – Schema for teachers, students, packages, enrollments, lessons, payments, receipts, materials
-- Safe to run multiple times (idempotent guards for tables and columns).

create extension if not exists "uuid-ossp";

-- Teachers
create table if not exists teachers (
  id uuid primary key default uuid_generate_v4(),
  name text not null,
  email text,
  phone text,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

-- Students
create table if not exists students (
  id uuid primary key default uuid_generate_v4(),
  name text not null,
  email text,
  phone text,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

-- Packages
create table if not exists lesson_packages (
  id uuid primary key default uuid_generate_v4(),
  name text not null,
  description text,
  total_lessons int not null check (total_lessons > 0),
  price_cents int not null check (price_cents >= 0),
  currency text not null default 'USD',
  valid_from timestamptz,
  valid_to timestamptz,
  is_active boolean not null default true,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

-- Enrollments (purchase of a package by a student)
create table if not exists enrollments (
  id uuid primary key default uuid_generate_v4(),
  student_id uuid not null references students(id) on delete cascade,
  package_id uuid not null references lesson_packages(id),
  purchase_date timestamptz not null default now(),
  lessons_remaining int not null,
  status text not null default 'active' check (status in ('active','completed','cancelled')),
  notes text,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

-- Lessons
create table if not exists lessons (
  id uuid primary key default uuid_generate_v4(),
  teacher_id uuid not null references teachers(id) on delete restrict,
  student_id uuid not null references students(id) on delete restrict,
  enrollment_id uuid references enrollments(id) on delete set null,
  start_at timestamptz not null,
  end_at timestamptz not null,
  location text,
  status text not null default 'scheduled' check (status in ('scheduled','completed','cancelled')),
  notes text,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

-- Lesson materials
create table if not exists lesson_materials (
  id uuid primary key default uuid_generate_v4(),
  lesson_id uuid not null references lessons(id) on delete cascade,
  title text not null,
  url text not null,
  notes text,
  created_at timestamptz not null default now()
);

-- Payments (for an enrollment)
create table if not exists payments (
  id uuid primary key default uuid_generate_v4(),
  enrollment_id uuid not null references enrollments(id) on delete cascade,
  amount_cents int not null check (amount_cents >= 0),
  currency text not null default 'USD',
  method text not null default 'other' check (method in ('cash','card','bank_transfer','other')),
  reference text,
  status text not null default 'paid' check (status in ('pending','paid','void')),
  paid_at timestamptz not null default now(),
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

-- Receipts (issued after payment)
create table if not exists receipts (
  id uuid primary key default uuid_generate_v4(),
  payment_id uuid not null references payments(id) on delete cascade,
  receipt_number text not null unique,
  issued_at timestamptz not null default now(),
  buyer_name text not null,
  buyer_email text,
  line_items jsonb not null default '[]'::jsonb,
  subtotal_cents int not null default 0,
  tax_cents int not null default 0,
  total_cents int not null default 0,
  currency text not null default 'USD',
  created_at timestamptz not null default now()
);

-- Derived helpers / views
create or replace view v_teacher_students as
  select distinct l.teacher_id, l.student_id
  from lessons l;

-- Triggers to keep updated_at in sync
create or replace function set_updated_at()
returns trigger as $$
begin
  new.updated_at = now();
  return new;
end;
$$ language plpgsql;

do $$ begin
  if not exists (select 1 from pg_trigger where tgname = 'set_updated_at_teachers') then
    create trigger set_updated_at_teachers before update on teachers
      for each row execute procedure set_updated_at();
  end if;
end $$;

do $$ begin
  if not exists (select 1 from pg_trigger where tgname = 'set_updated_at_students') then
    create trigger set_updated_at_students before update on students
      for each row execute procedure set_updated_at();
  end if;
end $$;

do $$ begin
  if not exists (select 1 from pg_trigger where tgname = 'set_updated_at_packages') then
    create trigger set_updated_at_packages before update on lesson_packages
      for each row execute procedure set_updated_at();
  end if;
end $$;

do $$ begin
  if not exists (select 1 from pg_trigger where tgname = 'set_updated_at_enrollments') then
    create trigger set_updated_at_enrollments before update on enrollments
      for each row execute procedure set_updated_at();
  end if;
end $$;

do $$ begin
  if not exists (select 1 from pg_trigger where tgname = 'set_updated_at_lessons') then
    create trigger set_updated_at_lessons before update on lessons
      for each row execute procedure set_updated_at();
  end if;
end $$;

do $$ begin
  if not exists (select 1 from pg_trigger where tgname = 'set_updated_at_payments') then
    create trigger set_updated_at_payments before update on payments
      for each row execute procedure set_updated_at();
  end if;
end $$;

-- Seed demo data (safe re-run)
insert into teachers (id, name, email) values
  ('11111111-1111-1111-1111-111111111111','Default Teacher','teacher@example.com')
on conflict (id) do nothing;

insert into students (id, name, email) values
  ('22222222-2222-2222-2222-222222222222','Default Student','student@example.com')
on conflict (id) do nothing;

insert into lesson_packages (id, name, description, total_lessons, price_cents, currency, is_active) values
  ('33333333-3333-3333-3333-333333333333','Starter (5 lessons)','Introductory pack',5,25000,'USD',true),
  ('44444444-4444-4444-4444-444444444444','Standard (10 lessons)','Most popular',10,45000,'USD',true)
on conflict (id) do nothing;

insert into enrollments (id, student_id, package_id, lessons_remaining, status) values
  ('55555555-5555-5555-5555-555555555555','22222222-2222-2222-2222-222222222222','33333333-3333-3333-3333-333333333333',5,'active')
on conflict (id) do nothing;

insert into payments (id, enrollment_id, amount_cents, currency, method, status) values
  ('66666666-6666-6666-6666-666666666666','55555555-5555-5555-5555-555555555555',25000,'USD','card','paid')
on conflict (id) do nothing;

insert into receipts (id, payment_id, receipt_number, buyer_name, line_items, subtotal_cents, tax_cents, total_cents, currency) values
  ('77777777-7777-7777-7777-777777777777','66666666-6666-6666-6666-666666666666','R-2025-0001','Default Student',
   jsonb_build_array(jsonb_build_object('description','Starter (5 lessons)','qty',1,'unit_price_cents',25000,'total_cents',25000)),
   25000,0,25000,'USD')
on conflict (id) do nothing;

