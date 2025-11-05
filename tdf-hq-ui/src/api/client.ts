import axios from 'axios'

export const apiClient = axios.create({
  baseURL: import.meta.env.VITE_API_BASE || 'http://localhost:8080',
  headers: {
    'Content-Type': 'application/json',
  },
  timeout: 10000, // 10 second timeout
})

// Add response interceptor for error handling
apiClient.interceptors.response.use(
  (response) => response,
  (error) => {
    if (error.response) {
      // Server responded with error status
      console.error('API Error:', error.response.status, error.response.data)
    } else if (error.request) {
      // Request was made but no response received
      console.error('Network Error: No response from server')
    } else {
      // Error in request setup
      console.error('Request Error:', error.message)
    }
    return Promise.reject(error)
  }
)

// Types for API responses
export interface PartyRole {
  role: 'AdminRole' | 'ManagerRole' | 'EngineerRole' | 'TeacherRole' | 
        'ReceptionRole' | 'AccountingRole' | 'ReadOnlyRole' | 
        'CustomerRole' | 'ArtistRole' | 'StudentRole'
}

export interface UserWithParty {
  uwpUserId: number
  uwpEmail?: string
  uwpName: string
  uwpRoles: PartyRole['role'][]
  uwpIsActive: boolean
  uwpLastLoginAt?: string
}

export interface UpdateRolesRequest {
  urrRoles: PartyRole['role'][]
}

export interface UpdateRoleResponse {
  urrSuccess: boolean
  urrMessage: string
  urrUser?: UserWithParty
}

// API functions
export const getUsers = async (): Promise<UserWithParty[]> => {
  const response = await apiClient.get<UserWithParty[]>('/api/users')
  return response.data
}

export const updateUserRoles = async (
  userId: number,
  roles: PartyRole['role'][]
): Promise<UpdateRoleResponse> => {
  const response = await apiClient.put<UpdateRoleResponse>(
    `/api/users/${userId}/roles`,
    { urrRoles: roles }
  )
  return response.data
}
