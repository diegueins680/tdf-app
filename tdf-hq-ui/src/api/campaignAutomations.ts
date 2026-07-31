import { get, post } from './client';

export type CampaignAutomationStatus = 'draft' | 'active' | 'paused' | 'completed';
export type CampaignEnrollmentStatus =
  | 'scheduled'
  | 'completed'
  | 'stopped'
  | 'replied'
  | 'converted';

export interface CampaignAutomationStep {
  position: number;
  delayDays: number;
  channel: 'whatsapp';
  providerTemplateName: string;
  languageCode: string;
  body: string;
  ctaPath: string;
}

export interface CampaignAutomationTemplate {
  key: string;
  name: string;
  objective: string;
  audience: string;
  offer: string;
  landingPath: string;
  steps: CampaignAutomationStep[];
}

export interface CampaignAutomation {
  id: number;
  campaignId: number;
  templateKey: string;
  name: string;
  objective?: string | null;
  status: CampaignAutomationStatus;
  startAt: string;
  dailyLimit: number;
  lastRunAt?: string | null;
  enrollmentCount: number;
  scheduledCount: number;
  sentCount: number;
  convertedCount: number;
  stoppedCount: number;
  failedCount: number;
  steps: CampaignAutomationStep[];
}

export interface CampaignEnrollment {
  id: number;
  partyId: number;
  partyName: string;
  phoneE164?: string | null;
  consentActive: boolean;
  status: CampaignEnrollmentStatus;
  nextStepPosition: number;
  nextRunAt: string;
  lastSentAt?: string | null;
  stopReason?: string | null;
}

export interface CampaignEnrollmentRejected {
  partyId: number;
  reason: string;
}

export interface CampaignEnrollmentResult {
  acceptedPartyIds: number[];
  rejected: CampaignEnrollmentRejected[];
}

export interface CampaignPreview {
  partyId: number;
  partyName: string;
  stepPosition: number;
  providerTemplateName: string;
  languageCode: string;
  renderedBody: string;
  ctaUrl: string;
}

const requirePositiveInteger = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${field} debe ser un entero positivo.`);
  }
  return value;
};

export const CampaignAutomations = {
  templates: () =>
    get<CampaignAutomationTemplate[]>('/ads/automation-templates'),

  list: () =>
    get<CampaignAutomation[]>('/ads/automations'),

  install: (templateKey: string, startAt?: string, dailyLimit = 20) =>
    post<CampaignAutomation>('/ads/automations', {
      templateKey,
      ...(startAt ? { startAt } : {}),
      dailyLimit,
    }),

  enroll: (automationId: number, partyIds: number[]) =>
    post<CampaignEnrollmentResult>(
      `/ads/automations/${requirePositiveInteger(automationId, 'automationId')}/enroll`,
      { partyIds },
    ),

  enrollments: (automationId: number) =>
    get<CampaignEnrollment[]>(
      `/ads/automations/${requirePositiveInteger(automationId, 'automationId')}/enrollments`,
    ),

  preview: (automationId: number) =>
    get<CampaignPreview[]>(
      `/ads/automations/${requirePositiveInteger(automationId, 'automationId')}/preview`,
    ),

  setStatus: (
    automationId: number,
    status: CampaignAutomationStatus,
    templatesApproved = false,
  ) =>
    post<CampaignAutomation>(
      `/ads/automations/${requirePositiveInteger(automationId, 'automationId')}/status`,
      {
        status,
        ...(status === 'active' ? { templatesApproved } : {}),
      },
    ),

  setEnrollmentStatus: (
    automationId: number,
    enrollmentId: number,
    status: Extract<CampaignEnrollmentStatus, 'scheduled' | 'converted' | 'stopped'>,
    reason?: string,
  ) =>
    post<CampaignEnrollment>(
      `/ads/automations/${requirePositiveInteger(automationId, 'automationId')}`
        + `/enrollments/${requirePositiveInteger(enrollmentId, 'enrollmentId')}/status`,
      {
        status,
        ...(reason?.trim() ? { reason: reason.trim() } : {}),
      },
    ),
};
