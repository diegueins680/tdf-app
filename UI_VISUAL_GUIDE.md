# UI Visual Guide - Multi-Role User Management

This document provides text-based visual representations of the user interface.

## Main User Management Page

```
┌────────────────────────────────────────────────────────────────────────┐
│  TDF Records - User Role Management                                    │
│                                                                         │
│  Manage user roles for the TDF Records platform. Users can have       │
│  multiple roles assigned.                                              │
└────────────────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────────────────┐
│ ID │ Name         │ Email           │ Phone         │ Roles    │ Actions│
├────┼──────────────┼─────────────────┼───────────────┼──────────┼────────┤
│ 1  │ Alice Admin  │ alice@tdf.com   │ +593987654321 │ [Admin]  │  ✏️   │
│    │              │                 │               │ [Manager]│        │
├────┼──────────────┼─────────────────┼───────────────┼──────────┼────────┤
│ 2  │ Bob Teacher  │ bob@tdf.com     │ +593987654322 │ [Teacher]│  ✏️   │
│    │              │                 │               │ [Artist] │        │
├────┼──────────────┼─────────────────┼───────────────┼──────────┼────────┤
│ 3  │ Carol Student│ carol@tdf.com   │ -             │ [Student]│  ✏️   │
├────┼──────────────┼─────────────────┼───────────────┼──────────┼────────┤
│ 4  │ Dave Manager │ dave@tdf.com    │ +593987654323 │ [Manager]│  ✏️   │
│    │              │                 │               │[Reception]        │
└────┴──────────────┴─────────────────┴───────────────┴──────────┴────────┘
```

### Role Chip Colors

```
[Admin]      - Red background, white text
[Manager]    - Blue background, white text
[Engineer]   - Light blue background, dark text
[Teacher]    - Green background, white text
[Reception]  - Purple background, white text
[Accounting] - Orange background, white text
[Artist]     - Blue background, white text
[Student]    - Gray background, dark text
[ReadOnly]   - Gray background, dark text
```

## Edit Role Dialog (Closed State)

When user clicks the edit icon (✏️), a modal dialog appears.

## Edit Role Dialog (Open State)

```
┌────────────────────────────────────────────────────────────┐
│  Edit Roles for Alice Admin                          ✕    │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  Roles                                                     │
│  ┌──────────────────────────────────────────────────────┐ │
│  │ [Admin] [Manager]                               ▼   │ │
│  └──────────────────────────────────────────────────────┘ │
│                                                            │
│                                                            │
├────────────────────────────────────────────────────────────┤
│                                    [Cancel]  [Save]        │
└────────────────────────────────────────────────────────────┘
```

## Edit Role Dialog (Dropdown Expanded)

When user clicks on the role selector:

```
┌────────────────────────────────────────────────────────────┐
│  Edit Roles for Alice Admin                          ✕    │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  Roles                                                     │
│  ┌──────────────────────────────────────────────────────┐ │
│  │ [Admin] [Manager]                               ▲   │ │
│  ├──────────────────────────────────────────────────────┤ │
│  │ ✓ Admin                                              │ │
│  │ ✓ Manager                                            │ │
│  │   Engineer                                           │ │
│  │   Teacher                                            │ │
│  │   Reception                                          │ │
│  │   Accounting                                         │ │
│  │   Artist                                             │ │
│  │   Student                                            │ │
│  │   ReadOnly                                           │ │
│  └──────────────────────────────────────────────────────┘ │
│                                                            │
├────────────────────────────────────────────────────────────┤
│                                    [Cancel]  [Save]        │
└────────────────────────────────────────────────────────────┘
```

## Edit Role Dialog (Multiple Selections)

User has selected: Teacher, Artist, and Student:

```
┌────────────────────────────────────────────────────────────┐
│  Edit Roles for Bob Teacher                          ✕    │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  Roles                                                     │
│  ┌──────────────────────────────────────────────────────┐ │
│  │ [Teacher] [Artist] [Student]                    ▼   │ │
│  └──────────────────────────────────────────────────────┘ │
│                                                            │
│                                                            │
├────────────────────────────────────────────────────────────┤
│                                    [Cancel]  [Save]        │
└────────────────────────────────────────────────────────────┘
```

## Loading State

While fetching users from API:

```
┌────────────────────────────────────────────────────────────────────────┐
│  TDF Records - User Role Management                                    │
│                                                                         │
│  Manage user roles for the TDF Records platform. Users can have       │
│  multiple roles assigned.                                              │
└────────────────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────────────────┐
│                                                                         │
│                              ⟳ Loading...                             │
│                                                                         │
└────────────────────────────────────────────────────────────────────────┘
```

## Saving State

While saving role changes:

```
┌────────────────────────────────────────────────────────────┐
│  Edit Roles for Alice Admin                          ✕    │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  Roles                                                     │
│  ┌──────────────────────────────────────────────────────┐ │
│  │ [Admin] [Manager] [Teacher]                     ▼   │ │
│  └──────────────────────────────────────────────────────┘ │
│                                                            │
│                                                            │
├────────────────────────────────────────────────────────────┤
│                          [Cancel]  [Saving... ⟳]          │
│                          (disabled)  (disabled)            │
└────────────────────────────────────────────────────────────┘
```

## Error State

When API call fails:

```
┌────────────────────────────────────────────────────────────────────────┐
│  TDF Records - User Role Management                                    │
│                                                                         │
│  Manage user roles for the TDF Records platform. Users can have       │
│  multiple roles assigned.                                              │
└────────────────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────────────────┐
│  ⚠ Error                                                          ✕   │
│  Failed to load users. Please try again.                              │
└────────────────────────────────────────────────────────────────────────┘
```

## Empty State (No Roles)

User with no roles assigned:

```
┌────────────────────────────────────────────────────────────────────────┐
│ ID │ Name         │ Email           │ Phone         │ Roles    │ Actions│
├────┼──────────────┼─────────────────┼───────────────┼──────────┼────────┤
│ 5  │ Eve New User │ eve@tdf.com     │ -             │ [No roles]  ✏️   │
└────┴──────────────┴─────────────────┴───────────────┴──────────┴────────┘
```

## Mobile View (Responsive)

On small screens, table becomes scrollable:

```
┌─────────────────────────────────────┐
│ TDF Records - User Management       │
│                                     │
│ Manage user roles...                │
└─────────────────────────────────────┘

← Scroll horizontally →

┌─────────────────────────────────────┐
│ID │ Name    │ Email   │ ... │ Actions│
├───┼─────────┼─────────┼─────┼────────┤
│ 1 │ Alice   │alice@...│ ... │   ✏️  │
│   │ Admin   │         │ ... │        │
├───┼─────────┼─────────┼─────┼────────┤
│ 2 │ Bob     │bob@...  │ ... │   ✏️  │
│   │ Teacher │         │ ... │        │
└───┴─────────┴─────────┴─────┴────────┘
```

## Role Chip Examples

### Single Role
```
┌────────────────┐
│ [Teacher]      │
└────────────────┘
```

### Multiple Roles (Wrapped)
```
┌────────────────┐
│ [Admin]        │
│ [Manager]      │
│ [Teacher]      │
│ [Artist]       │
└────────────────┘
```

### Multiple Roles (Inline)
```
┌─────────────────────────────────────┐
│ [Admin] [Manager] [Teacher] [Artist]│
└─────────────────────────────────────┘
```

## Interactive States

### Button States

**Normal:**
```
┌─────────┐
│  Save   │
└─────────┘
```

**Hover:**
```
┌─────────┐
│  Save   │  (darker blue)
└─────────┘
```

**Disabled:**
```
┌─────────┐
│  Save   │  (grayed out)
└─────────┘
```

**Loading:**
```
┌──────────────┐
│ Saving... ⟳ │
└──────────────┘
```

### Role Chip States

**Selected in Dropdown:**
```
✓ Admin       (checkmark, highlighted)
```

**Not Selected:**
```
  Engineer    (no checkmark, normal)
```

## User Flows

### Flow 1: Add a Role

```
1. Initial State:
   User: Bob Teacher
   Roles: [Teacher]

2. Click edit icon:
   Dialog opens
   Dropdown shows: ✓ Teacher

3. Click dropdown:
   List appears
   Select "Artist"

4. After selection:
   Chips show: [Teacher] [Artist]

5. Click Save:
   Button shows "Saving..."

6. Success:
   Dialog closes
   Table shows: [Teacher] [Artist]
```

### Flow 2: Remove a Role

```
1. Initial State:
   User: Alice Admin
   Roles: [Admin] [Manager]

2. Click edit icon:
   Dialog opens
   Chips show: [Admin] [Manager]

3. Click dropdown:
   List shows: ✓ Admin ✓ Manager

4. Deselect Manager:
   Chips now: [Admin]

5. Click Save:
   Success

6. Final State:
   Table shows: [Admin]
```

### Flow 3: Replace All Roles

```
1. Initial State:
   User: Carol Student
   Roles: [Student]

2. Open dialog, deselect Student

3. Select: Teacher, Artist, Reception

4. Save

5. Final State:
   Roles: [Teacher] [Artist] [Reception]
```

## Color Palette

```
Primary Blue:    #1976d2  (Manager, Artist, buttons)
Error Red:       #d32f2f  (Admin)
Success Green:   #2e7d32  (Teacher)
Warning Orange:  #ed6c02  (Accounting)
Info Light Blue: #0288d1  (Engineer)
Secondary Purple:#9c27b0  (Reception)
Gray:            #757575  (Student, ReadOnly)
```

## Typography

```
Heading (h3):    32px, bold
Body:            16px, regular
Table Headers:   14px, medium
Table Cells:     14px, regular
Chip Labels:     13px, medium
Button Text:     14px, medium
```

## Spacing

```
Page Padding:    24px
Table Padding:   16px
Dialog Padding:  24px
Chip Gap:        4px
Button Gap:      8px
Section Gap:     16px
```

## Accessibility

### Keyboard Navigation

```
Tab Order:
1. Edit Icon (User 1)
2. Edit Icon (User 2)
3. ...
4. Edit Icon (User N)

In Dialog:
1. Close button (X)
2. Role dropdown
3. Cancel button
4. Save button
```

### Screen Reader Announcements

```
"User table with 4 users"
"Edit roles for Alice Admin"
"Role dropdown, Admin and Manager selected"
"Saving roles..."
"Roles updated successfully"
```

### Focus States

```
Focused Button:
┌─────────┐
│  Save   │  (blue outline)
└─────────┘

Focused Dropdown:
┌──────────────────────┐
│ [Admin] [Manager] ▼ │  (blue outline)
└──────────────────────┘
```

## Animation States

### Dialog Appearance
```
Frame 1: Fade in background (opacity 0 → 0.5)
Frame 2: Slide up dialog (translateY 20px → 0)
Total duration: 200ms
```

### Chip Addition
```
Frame 1: Scale in (scale 0.8 → 1.0)
Total duration: 150ms
```

### Button Click
```
Frame 1: Scale down (0.95)
Frame 2: Scale up (1.0)
Total duration: 100ms
```

## Summary

This UI provides:
- ✅ Clear visual hierarchy
- ✅ Intuitive role management
- ✅ Color-coded roles for quick identification
- ✅ Responsive design for all screen sizes
- ✅ Accessible keyboard navigation
- ✅ Loading and error states
- ✅ Smooth animations
- ✅ Material Design principles

The interface is designed to be:
- **Simple**: One-click editing
- **Clear**: Visual feedback at every step
- **Fast**: No page reloads
- **Accessible**: Keyboard and screen reader support
- **Professional**: Material-UI components
