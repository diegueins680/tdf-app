
import { useState } from "react";
import { ScheduleModal } from "../components/ScheduleModal";

// Minimal wiring example. Integrate with your existing Kanban DnD.
export function withSopEnforcement(Kanban:any, api:any){
  return function EnforcedKanban(props:any){
    const [scheduleOpen,setScheduleOpen]=useState(false);
    const [projectId,setProjectId]=useState<number|undefined>(undefined);

    const onBeforeMove = async (card:any, toStage:string) => {
      if (toStage === "Schedule"){
        setProjectId(card.id);
        setScheduleOpen(true);
        return false; // block move until modal submits
      }
      return true;
    };

    const onScheduled = () => {
      setScheduleOpen(false);
      // Optionally refresh board
      if (props.onRefresh) props.onRefresh();
    };

    return (<>
      <Kanban {...props} onBeforeMove={onBeforeMove} />
      {projectId && <ScheduleModal open={scheduleOpen} onClose={()=>setScheduleOpen(false)} projectId={projectId} onOk={onScheduled} api={api} />}
    </>);
  }
}
