# TDF Application Features

This document describes all the features implemented in the TDF Records Management Platform.

## 📊 Dashboard

**Route:** `/dashboard`

The Analytics Dashboard provides a comprehensive overview of your business metrics at a glance.

### Key Metrics

- **Total Revenue** - Overall revenue with monthly breakdown and growth percentage
- **Utilization Rate** - Percentage of booked time across all studios and rooms
- **Active Packages** - Number of active package purchases with usage statistics
- **Accounts Receivable** - Outstanding invoices with overdue tracking

### Detailed Analytics

#### Revenue by Service
Visual breakdown showing revenue distribution across:
- Recording
- Classes
- Mixing
- Rehearsal
- Mastering

Each service shows dollar amount and percentage of total revenue with progress bars.

#### Room Utilization
Tracks usage of each studio/room:
- Total hours booked
- Utilization percentage
- Color-coded indicators (green >75%, yellow >50%, red <50%)

#### AR Aging Report
Tracks outstanding payments by period:
- 0-30 days (current)
- 31-60 days
- 61-90 days
- 90+ days (overdue)

#### Maintenance Alerts
Real-time alerts for equipment maintenance:
- Overdue maintenance items (red)
- Due soon items (yellow)
- Shows item name, category, and due date

## 📦 Package Management

**Route:** `/packages`

Manage lesson packages, recording packages, and other service bundles.

### Features

- **Package Catalog** - Card-based view of all available packages
- **Create Packages** - Configure new packages with:
  - Name and service type
  - Price in USD
  - Units (hours, sessions, or songs)
  - Expiration period (in days)
  - Refund policy (credit only, full refund, or no refund)
  - Description

### Package Types Supported

- **Classes** - Music lessons (guitar, bass, voice, etc.)
- **Recording** - Studio recording time
- **Mixing** - Audio mixing services
- **Mastering** - Mastering services
- **Rehearsal** - Rehearsal room time

### Package Tracking

Each package purchase tracks:
- Total units purchased
- Units used
- Units remaining
- Purchase date
- Expiration date
- Status (active, expired, depleted)

## 💰 Invoice & Payment Management

**Route:** `/invoices`

Complete invoicing and payment tracking system.

### Invoice Features

- **Invoice List** - View all invoices with:
  - Invoice number
  - Customer name
  - Issue and due dates
  - Total amount, paid amount, and balance
  - Status (draft, issued, paid, overdue, cancelled)

- **Create Invoices** - Multi-line item invoicing:
  - Add unlimited line items
  - Each item has description, quantity, and unit price
  - Automatic total calculation
  - Notes field for additional information

### Payment Processing

- **Record Payments** - Track payments with:
  - Amount
  - Payment method (cash, bank transfer, card/POS)
  - Reference/transaction ID
  - Payment notes
  - Automatic balance updates

- **Payment Methods Supported:**
  - Cash
  - Bank Transfer
  - Card/POS (Point of Sale)

### Invoice Status Workflow

1. **Draft** - Invoice created but not sent
2. **Issued** - Invoice sent to customer
3. **Paid** - Fully paid
4. **Overdue** - Past due date with outstanding balance
5. **Cancelled** - Cancelled invoice

## 🎸 Inventory & Equipment Management

**Route:** `/inventory`

Comprehensive inventory tracking system for studios, equipment, and consumables.

### Three Main Views

#### 1. All Items
Complete inventory list showing:
- Item name and category
- Serial number (if applicable)
- Current location
- Status (available, checked out, maintenance, retired)
- Condition (excellent, good, fair, poor)

#### 2. Checked Out
Active equipment loans showing:
- Item and borrower
- Checkout date
- Expected return date
- Quick check-in button
- Checkout notes

#### 3. Maintenance Due
Equipment requiring maintenance:
- Items overdue for maintenance
- Items due soon
- Last maintenance date
- Next scheduled maintenance

### Inventory Features

- **Add Items** - Create inventory records with:
  - Name and category
  - Serial number tracking
  - Location tracking
  - Condition assessment
  - Purchase date
  - Notes

- **Check-Out System** - Lend equipment to customers/staff:
  - Select item and borrower
  - Set expected return date
  - Add checkout notes
  - Automatic status update

- **Check-In** - Return equipment:
  - One-click check-in
  - Automatic availability status update
  - Return date logging

- **Maintenance Scheduling** - Track equipment health:
  - Schedule maintenance dates
  - Automatic alerts for due items
  - Maintenance history tracking

### Status Types

- **Available** - Ready to use or check out
- **Checked Out** - Currently loaned to someone
- **Maintenance** - Being serviced or repaired
- **Retired** - No longer in active use

### Condition Grades

- **Excellent** - Like new, perfect condition
- **Good** - Minor wear, fully functional
- **Fair** - Noticeable wear, may need attention
- **Poor** - Significant wear, needs repair/replacement

## 👥 Parties Management

**Route:** `/parties`

Customer relationship management for all contacts.

### Features

- **Party List** - View all customers, teachers, and staff
- **Create/Edit** - Manage party information:
  - Legal name and display name
  - Organization flag
  - Tax ID
  - Contact information (email, phone)
  - WhatsApp and Instagram handles
  - Emergency contact
  - Notes

### Party Types

The system uses a unified "Party" model that can represent:
- Customers/Students
- Teachers
- Staff
- Bands/Organizations
- Vendors

## 📅 Bookings & Scheduling

**Route:** `/bookings`

Calendar-based booking system for studios and rooms.

### Features

- **FullCalendar Integration** - Visual calendar interface
- **Multiple Views** - Day, week, and month views
- **Create Bookings** - Schedule sessions with:
  - Title
  - Start and end times
  - Status
  - Notes

### Supported Resources

- Studios (A, B, etc.)
- Rehearsal rooms
- Classrooms
- Control rooms
- Booth spaces

## 📋 Kanban Pipelines

**Route:** `/kanban`

Service workflow management with drag-and-drop boards.

### Pipeline Types

#### Recording Pipeline
Inquiry → Quoted → Scheduled → In Session → Editing → Approved → Delivered

#### Mixing Pipeline
Brief → Prep → v1 Sent → Revisions → Approved → Delivered

#### Mastering Pipeline
Brief → v1 → Revisions → Approved → DDP Delivered

#### Classes Pipeline
Enrolled → Scheduled → Attended → Make-up Needed → Completed

#### Event Production Pipeline
Lead → Proposal → Confirmed → Pre-Prod → Onsite → Post-Prod → Settled

### Features

- **Drag & Drop** - Move items between stages
- **Visual Workflow** - See all projects at a glance
- **Status Tracking** - Monitor progress through pipelines

## 🔐 Security & Access Control

### Planned Features

- Role-based access control (RBAC)
- User authentication with JWT
- Google OAuth integration
- Permission management per role:
  - Admin - Full access
  - Manager - Operations and reporting
  - Teacher - Classes and students
  - Reception - Bookings and customers
  - Accounting - Financial data only
  - ReadOnly - View-only access

## 🌐 API Integration

All features are designed to integrate with the Haskell backend via RESTful APIs.

### API Design Principles

- **RESTful** - Standard HTTP methods (GET, POST, PUT, DELETE)
- **JSON** - All data in JSON format
- **Type-Safe** - Full TypeScript type definitions
- **Error Handling** - Proper error messages and status codes
- **Pagination** - Support for large datasets (planned)

### Mock Data

The Dashboard currently uses mock data to demonstrate functionality. When the backend is implemented, these will be replaced with real API calls.

## 🎨 User Interface

### Design System

- **Material-UI (MUI)** - Professional, accessible components
- **Responsive** - Works on desktop, tablet, and mobile
- **Dark/Light Mode** - Theme toggle support (planned)
- **Color-Coded Status** - Visual indicators for quick understanding
- **Progress Bars** - Visual metrics representation
- **Card Layouts** - Organized information display

### User Experience

- **Intuitive Navigation** - Clear top navigation bar
- **Modal Dialogs** - For create/edit operations
- **Inline Actions** - Quick actions in tables
- **Real-time Updates** - Via React Query
- **Loading States** - Clear feedback during operations
- **Error Messages** - User-friendly error handling

## 📈 Future Enhancements

### High Priority

- [ ] Backend API implementation in Haskell
- [ ] User authentication and authorization
- [ ] Search and filtering across all pages
- [ ] Pagination for large datasets
- [ ] Data export (CSV, PDF, XLSX)

### Medium Priority

- [ ] Package usage tracking and history
- [ ] Invoice PDF generation
- [ ] Email integration for invoices
- [ ] QR code generation for inventory
- [ ] Advanced reporting and charts
- [ ] Calendar sync (Google Calendar)

### Low Priority

- [ ] Mobile app (React Native/Expo)
- [ ] SMS notifications
- [ ] Automated reminders
- [ ] Recurring invoices
- [ ] Multi-currency support
- [ ] Tax calculation automation

## 🚀 Getting Started

1. **Install Dependencies**
   ```bash
   npm install
   ```

2. **Configure Environment**
   ```bash
   cp tdf-hq-ui/.env.example tdf-hq-ui/.env
   # Edit .env with your API base URL
   ```

3. **Run Development Server**
   ```bash
   npm run dev:ui
   ```

4. **Access Application**
   Open http://localhost:5173 in your browser

## 🛠️ Technology Stack

- **React 18** - UI framework
- **TypeScript** - Type safety
- **Vite** - Build tool and dev server
- **Material-UI v6** - Component library
- **React Query** - Server state management
- **React Hook Form** - Form handling
- **React Router** - Navigation
- **FullCalendar** - Calendar component
- **Luxon** - Date/time handling
- **i18next** - Internationalization (planned)

---

**TDF Records** - Empowering creativity through technology 🎵
