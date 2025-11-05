
import { useQuery } from "@tanstack/react-query";

export function useSopCompliance(api:any, projectId:number, stage:string){
  return useQuery({
    queryKey: ["sopCompliance", projectId, stage],
    queryFn: async () => {
      const { data } = await api.get(`/projects/${projectId}/sop/compliance`, { params: { stage } });
      return data; // { missing: string[] }
    }
  });
}
