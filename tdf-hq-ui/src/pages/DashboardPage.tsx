import { useQuery } from '@tanstack/react-query';
import {
  Box,
  Card,
  CardContent,
  Grid,
  Typography,
  Alert,
  LinearProgress,
  Chip,
} from '@mui/material';
import {
  TrendingUp as TrendingUpIcon,
  AttachMoney as MoneyIcon,
  EventAvailable as EventIcon,
  Warning as WarningIcon,
  Build as BuildIcon,
} from '@mui/icons-material';

// Mock analytics API - would be implemented in a real backend
const Analytics = {
  getRevenueSummary: () =>
    Promise.resolve({
      totalRevenue: 45000,
      monthlyRevenue: 5600,
      growth: 12.5,
      byService: [
        { service: 'Recording', revenue: 15000, percentage: 33 },
        { service: 'Classes', revenue: 12000, percentage: 27 },
        { service: 'Mixing', revenue: 10000, percentage: 22 },
        { service: 'Rehearsal', revenue: 5000, percentage: 11 },
        { service: 'Mastering', revenue: 3000, percentage: 7 },
      ],
    }),
  
  getUtilizationStats: () =>
    Promise.resolve({
      totalBookings: 234,
      utilizationRate: 78,
      topRooms: [
        { room: 'Studio A', hours: 145, rate: 85 },
        { room: 'Studio B', hours: 120, rate: 70 },
        { room: 'Control Room', hours: 98, rate: 58 },
        { room: 'Rehearsal 1', hours: 87, rate: 51 },
      ],
    }),
  
  getPackageStats: () =>
    Promise.resolve({
      activePackages: 42,
      expiringNextMonth: 8,
      expiredThisMonth: 3,
      totalValue: 21000,
      averageUsage: 65,
    }),
  
  getARStats: () =>
    Promise.resolve({
      totalOutstanding: 8500,
      overdue: 2100,
      current: 6400,
      aging: [
        { period: '0-30 days', amount: 6400 },
        { period: '31-60 days', amount: 1300 },
        { period: '61-90 days', amount: 600 },
        { period: '90+ days', amount: 200 },
      ],
    }),
  
  getMaintenanceAlerts: () =>
    Promise.resolve({
      overdue: 3,
      dueSoon: 7,
      items: [
        { name: 'Prophet 08', category: 'Synthesizer', dueDate: '2025-10-15', status: 'overdue' },
        { name: 'Moog Subsequent 37', category: 'Synthesizer', dueDate: '2025-11-01', status: 'overdue' },
        { name: 'SVT Bass Amp', category: 'Amplifier', dueDate: '2025-11-08', status: 'due_soon' },
      ],
    }),
};

export default function DashboardPage() {
  const { data: revenue, isLoading: revenueLoading } = useQuery({
    queryKey: ['analytics', 'revenue'],
    queryFn: Analytics.getRevenueSummary,
  });

  const { data: utilization, isLoading: utilizationLoading } = useQuery({
    queryKey: ['analytics', 'utilization'],
    queryFn: Analytics.getUtilizationStats,
  });

  const { data: packages, isLoading: packagesLoading } = useQuery({
    queryKey: ['analytics', 'packages'],
    queryFn: Analytics.getPackageStats,
  });

  const { data: ar, isLoading: arLoading } = useQuery({
    queryKey: ['analytics', 'ar'],
    queryFn: Analytics.getARStats,
  });

  const { data: maintenance, isLoading: maintenanceLoading } = useQuery({
    queryKey: ['analytics', 'maintenance'],
    queryFn: Analytics.getMaintenanceAlerts,
  });

  return (
    <Box>
      <Typography variant="h4" gutterBottom>
        Analytics Dashboard
      </Typography>

      <Grid container spacing={3}>
        {/* Revenue Summary */}
        <Grid item xs={12} md={6} lg={3}>
          <Card>
            <CardContent>
              <Box sx={{ display: 'flex', alignItems: 'center', mb: 2 }}>
                <MoneyIcon color="primary" sx={{ mr: 1 }} />
                <Typography variant="h6">Total Revenue</Typography>
              </Box>
              {revenueLoading ? (
                <LinearProgress />
              ) : (
                <>
                  <Typography variant="h4">${revenue?.totalRevenue.toLocaleString()}</Typography>
                  <Typography variant="body2" color="text.secondary" sx={{ mt: 1 }}>
                    This Month: ${revenue?.monthlyRevenue.toLocaleString()}
                  </Typography>
                  <Chip
                    label={`+${revenue?.growth}%`}
                    color="success"
                    size="small"
                    sx={{ mt: 1 }}
                  />
                </>
              )}
            </CardContent>
          </Card>
        </Grid>

        {/* Utilization */}
        <Grid item xs={12} md={6} lg={3}>
          <Card>
            <CardContent>
              <Box sx={{ display: 'flex', alignItems: 'center', mb: 2 }}>
                <EventIcon color="primary" sx={{ mr: 1 }} />
                <Typography variant="h6">Utilization</Typography>
              </Box>
              {utilizationLoading ? (
                <LinearProgress />
              ) : (
                <>
                  <Typography variant="h4">{utilization?.utilizationRate}%</Typography>
                  <Typography variant="body2" color="text.secondary" sx={{ mt: 1 }}>
                    {utilization?.totalBookings} total bookings
                  </Typography>
                  <LinearProgress
                    variant="determinate"
                    value={utilization?.utilizationRate || 0}
                    sx={{ mt: 2 }}
                  />
                </>
              )}
            </CardContent>
          </Card>
        </Grid>

        {/* Package Stats */}
        <Grid item xs={12} md={6} lg={3}>
          <Card>
            <CardContent>
              <Box sx={{ display: 'flex', alignItems: 'center', mb: 2 }}>
                <TrendingUpIcon color="primary" sx={{ mr: 1 }} />
                <Typography variant="h6">Active Packages</Typography>
              </Box>
              {packagesLoading ? (
                <LinearProgress />
              ) : (
                <>
                  <Typography variant="h4">{packages?.activePackages}</Typography>
                  <Typography variant="body2" color="text.secondary" sx={{ mt: 1 }}>
                    Avg Usage: {packages?.averageUsage}%
                  </Typography>
                  {(packages?.expiringNextMonth || 0) > 0 && (
                    <Alert severity="warning" sx={{ mt: 1 }}>
                      {packages?.expiringNextMonth} expiring soon
                    </Alert>
                  )}
                </>
              )}
            </CardContent>
          </Card>
        </Grid>

        {/* AR Summary */}
        <Grid item xs={12} md={6} lg={3}>
          <Card>
            <CardContent>
              <Box sx={{ display: 'flex', alignItems: 'center', mb: 2 }}>
                <WarningIcon color="primary" sx={{ mr: 1 }} />
                <Typography variant="h6">Accounts Receivable</Typography>
              </Box>
              {arLoading ? (
                <LinearProgress />
              ) : (
                <>
                  <Typography variant="h4">${ar?.totalOutstanding.toLocaleString()}</Typography>
                  <Typography variant="body2" color="text.secondary" sx={{ mt: 1 }}>
                    Current: ${ar?.current.toLocaleString()}
                  </Typography>
                  {(ar?.overdue || 0) > 0 && (
                    <Alert severity="error" sx={{ mt: 1 }}>
                      ${ar?.overdue.toLocaleString()} overdue
                    </Alert>
                  )}
                </>
              )}
            </CardContent>
          </Card>
        </Grid>

        {/* Revenue by Service */}
        <Grid item xs={12} md={6}>
          <Card>
            <CardContent>
              <Typography variant="h6" gutterBottom>
                Revenue by Service
              </Typography>
              {revenueLoading ? (
                <LinearProgress />
              ) : (
                <Box>
                  {revenue?.byService.map((item) => (
                    <Box key={item.service} sx={{ mb: 2 }}>
                      <Box sx={{ display: 'flex', justifyContent: 'space-between', mb: 0.5 }}>
                        <Typography variant="body2">{item.service}</Typography>
                        <Typography variant="body2">
                          ${item.revenue.toLocaleString()} ({item.percentage}%)
                        </Typography>
                      </Box>
                      <LinearProgress
                        variant="determinate"
                        value={item.percentage}
                        sx={{ height: 8, borderRadius: 4 }}
                      />
                    </Box>
                  ))}
                </Box>
              )}
            </CardContent>
          </Card>
        </Grid>

        {/* Room Utilization */}
        <Grid item xs={12} md={6}>
          <Card>
            <CardContent>
              <Typography variant="h6" gutterBottom>
                Room Utilization
              </Typography>
              {utilizationLoading ? (
                <LinearProgress />
              ) : (
                <Box>
                  {utilization?.topRooms.map((room) => (
                    <Box key={room.room} sx={{ mb: 2 }}>
                      <Box sx={{ display: 'flex', justifyContent: 'space-between', mb: 0.5 }}>
                        <Typography variant="body2">{room.room}</Typography>
                        <Typography variant="body2">
                          {room.hours}h ({room.rate}%)
                        </Typography>
                      </Box>
                      <LinearProgress
                        variant="determinate"
                        value={room.rate}
                        sx={{ height: 8, borderRadius: 4 }}
                        color={room.rate > 75 ? 'success' : room.rate > 50 ? 'warning' : 'error'}
                      />
                    </Box>
                  ))}
                </Box>
              )}
            </CardContent>
          </Card>
        </Grid>

        {/* AR Aging */}
        <Grid item xs={12} md={6}>
          <Card>
            <CardContent>
              <Typography variant="h6" gutterBottom>
                AR Aging
              </Typography>
              {arLoading ? (
                <LinearProgress />
              ) : (
                <Box>
                  {ar?.aging.map((item) => (
                    <Box
                      key={item.period}
                      sx={{
                        display: 'flex',
                        justifyContent: 'space-between',
                        mb: 1,
                        pb: 1,
                        borderBottom: '1px solid #eee',
                      }}
                    >
                      <Typography variant="body2">{item.period}</Typography>
                      <Typography variant="body2" fontWeight="medium">
                        ${item.amount.toLocaleString()}
                      </Typography>
                    </Box>
                  ))}
                </Box>
              )}
            </CardContent>
          </Card>
        </Grid>

        {/* Maintenance Alerts */}
        <Grid item xs={12} md={6}>
          <Card>
            <CardContent>
              <Box sx={{ display: 'flex', alignItems: 'center', mb: 2 }}>
                <BuildIcon color="error" sx={{ mr: 1 }} />
                <Typography variant="h6">Maintenance Alerts</Typography>
              </Box>
              {maintenanceLoading ? (
                <LinearProgress />
              ) : (
                <Box>
                  <Alert severity="error" sx={{ mb: 2 }}>
                    {maintenance?.overdue} overdue, {maintenance?.dueSoon} due soon
                  </Alert>
                  {maintenance?.items.map((item, idx) => (
                    <Box
                      key={idx}
                      sx={{
                        display: 'flex',
                        justifyContent: 'space-between',
                        alignItems: 'center',
                        mb: 1,
                        pb: 1,
                        borderBottom: idx < (maintenance?.items.length || 0) - 1 ? '1px solid #eee' : 'none',
                      }}
                    >
                      <Box>
                        <Typography variant="body2" fontWeight="medium">
                          {item.name}
                        </Typography>
                        <Typography variant="caption" color="text.secondary">
                          {item.category}
                        </Typography>
                      </Box>
                      <Chip
                        label={item.dueDate}
                        color={item.status === 'overdue' ? 'error' : 'warning'}
                        size="small"
                      />
                    </Box>
                  ))}
                </Box>
              )}
            </CardContent>
          </Card>
        </Grid>
      </Grid>
    </Box>
  );
}
