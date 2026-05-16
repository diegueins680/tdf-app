import * as React from 'react';
import { Routes, Route } from 'react-router-dom';
import PackageList from '../features/packages/PackageList';
import Payments from '../features/payments/Payments';
import ReceiptView from '../features/receipts/ReceiptView';
import TeacherLessons from '../features/lessons/TeacherLessons';
import StudentsByTeacher from '../features/students/StudentsByTeacher';
import StudentLessons from '../features/lessons/StudentLessons';

export function TdfRoutes() {
  return (
    <Routes>
      <Route path="/packages" element={<PackageList />} />
      <Route path="/payments" element={<Payments />} />
      <Route path="/finance/receipts/:receiptId" element={<ReceiptView />} />
      <Route path="/teachers/:teacherId/lessons" element={<TeacherLessons />} />
      <Route path="/teachers/:teacherId/students" element={<StudentsByTeacher />} />
      <Route path="/students/:studentId/lessons" element={<StudentLessons />} />
    </Routes>
  );
}
