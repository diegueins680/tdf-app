import { onCLS, onFCP, onINP, onLCP, onTTFB, type Metric } from 'web-vitals';

import type { AnalyticsClient } from './posthog';

function roundMetricValue(value: number): number {
  return Math.round(value * 100) / 100;
}

/**
 * Reports standard Web Vitals through the existing privacy-reviewed analytics
 * client. No URLs, form values, or user-generated content are attached.
 */
export function startWebVitalsTracking(analytics: AnalyticsClient): void {
  if (!analytics.ready || typeof window === 'undefined') return;

  const report = (metric: Metric) => {
    analytics.capture('web_vital', {
      metric: metric.name,
      value: roundMetricValue(metric.value),
      delta: roundMetricValue(metric.delta),
      rating: metric.rating,
      metric_id: metric.id,
      navigation_type: metric.navigationType,
      pathname: window.location.pathname,
    });
  };

  onCLS(report);
  onFCP(report);
  onINP(report);
  onLCP(report);
  onTTFB(report);
}
