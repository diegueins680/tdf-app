# Millisecond contract shared by the audio pipeline and preview-only worker.
# null/null retains the documented automatic selection; explicit ranges never clamp.
def integer: type == "number" and floor == .;
if ($sourceDurationMs | integer) and $sourceDurationMs > 0
  and ($start == null or (($start | integer) and $start >= 0))
  and ($duration == null or (($duration | integer) and $duration > 0))
  and ($start == null or $duration != null)
then
  (if $duration == null then
    {startMs:(if $sourceDurationMs > 90000 then 30000 else 0 end),
     durationMs:([$sourceDurationMs,30000] | min), selection:"auto"}
   else {startMs:($start // 0),durationMs:$duration,selection:"explicit"} end)
  | if .startMs < $sourceDurationMs and .durationMs <= ($sourceDurationMs - .startMs)
    then . else error("previewStartMs/previewDurationMs exceed the inspected audio duration; select a range inside the track") end
else error("previewStartMs/previewDurationMs must be integer milliseconds; start requires a positive duration") end
