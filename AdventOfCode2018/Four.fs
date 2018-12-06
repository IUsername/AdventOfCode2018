﻿module Four

open System
open System.Collections.Generic

type GuardAction =
    | Wakes
    | Sleeps
    | StartShift of int

type GuardEvent = {time:DateTime; action:GuardAction}

type ShiftContext = {id:int; minutes:int }

type StampedGuardEvent = {id:int; event:GuardEvent}

type MostSleepyMinuteById = {id:int; minute:int; freq: int}

let parseEvent text = 
    let parseDateTime t =
        DateTime.ParseExact(t, "yyyy-MM-dd HH:mm", null) 
    match text with
    | Parsing.Regex @"\[(.+)\]\sfalls asleep\b" [t] -> 
        Some {GuardEvent.time = parseDateTime t; action = GuardAction.Sleeps}
    | Parsing.Regex @"\[(.+)\]\swakes up\b" [t] -> 
        Some {GuardEvent.time = parseDateTime t; action = GuardAction.Wakes}
    | Parsing.Regex @"\[(.+)\]\sGuard \#(\d+)\sbegins shift\b" [t;id] -> 
        Some {GuardEvent.time = parseDateTime t; action = GuardAction.StartShift (int id)}
    | _ -> None

let parseEvents (lines: seq<string>) = 
    Seq.map parseEvent lines |> Seq.choose id

let stampWithId (orderedEvents:seq<GuardEvent>) =
    let rec stampEventsEnumerator (id:int) (e:IEnumerator<GuardEvent>) = 
        seq {
            match e.MoveNext() with
            | false -> yield! []
            | true ->   let event = e.Current
                        match event.action with
                        | StartShift(i) ->
                            yield {id=i; event = event}
                            yield! stampEventsEnumerator i e
                        | Sleeps | Wakes -> 
                            yield {id=id; event = event}
                            yield! stampEventsEnumerator id e
            }
    orderedEvents.GetEnumerator() |> stampEventsEnumerator 0

let minutesAsleepPerShiftSeq (events:StampedGuardEvent list) =
    let rec minutesAsleepPerShift (s:ShiftContext) (fellAsleep:DateTime option) (events:StampedGuardEvent list) = 
        seq {
            match events with
            | [] -> yield! []
            | h::t ->   let time = h.event.time
                        match h.event.action with
                        | StartShift(i) -> 
                            if s.minutes > 0 then yield s
                            yield! minutesAsleepPerShift {ShiftContext.id = i; minutes = 0} None t
                        | Sleeps -> yield! minutesAsleepPerShift s (Some time) t
                        | Wakes -> yield! minutesAsleepPerShift {ShiftContext.id = s.id; minutes = s.minutes + (time - fellAsleep.Value).Minutes} None t
            }
    events |> minutesAsleepPerShift {id=0; minutes=0} None

let sumShifts (shifts:seq<ShiftContext>) = 
    shifts |> Seq.groupBy (fun s -> s.id) |> Seq.map (fun (id,s) -> {ShiftContext.id = id; minutes = s |> Seq.sumBy (fun s -> s.minutes) })

let toMinuteAsleepSeq (events:StampedGuardEvent list) =
    let genMinutesBetweenTimes (s:DateTime) (e:DateTime) = 
        let startMin = s.Minute
        let endMin = e.Minute
        seq {
            for i in [startMin..endMin] do
                yield i
        }
    let rec minuteAsleepPerShift (fellAsleep:DateTime option) (events:StampedGuardEvent list) = 
        seq {
            match events with            
            | h::t ->   let time = h.event.time
                        match h.event.action with
                        | StartShift(_) -> yield! minuteAsleepPerShift None t
                        | Sleeps -> yield! minuteAsleepPerShift (Some time) t
                        | Wakes -> 
                            yield! genMinutesBetweenTimes fellAsleep.Value time
                            yield! minuteAsleepPerShift None t
            | [] -> yield! []
            }
    events |> minuteAsleepPerShift None

let findMostSleepyMinuteById id (events:StampedGuardEvent list) =
    let mostSleepyMinute (minutes:seq<int>) =
        minutes |> Seq.countBy (fun m -> m) |> Seq.sortByDescending (fun (_,t) -> t) |> Seq.tryHead          
    let filterEventsById id (events:StampedGuardEvent list) = 
        events |> List.filter (fun s -> s.id = id)
    let r = filterEventsById id events |> toMinuteAsleepSeq |> mostSleepyMinute
    let (minute,freq) = defaultArg r (0,0)
    {MostSleepyMinuteById.id = id; minute = minute ; freq = freq}

let distinctIds (events:StampedGuardEvent list) =
    events |> List.map (fun e -> e.id) |> List.distinct

let sleepyPerGuard (events:StampedGuardEvent list) =
    let ids = distinctIds events
    ids |> List.map (fun i -> findMostSleepyMinuteById i events)
    

let dataSet = @"
[1518-10-14 00:05] falls asleep
[1518-09-13 00:12] falls asleep
[1518-03-23 00:26] falls asleep
[1518-03-10 00:48] wakes up
[1518-09-01 23:56] Guard #1019 begins shift
[1518-06-15 00:53] wakes up
[1518-05-01 00:53] wakes up
[1518-04-22 00:52] wakes up
[1518-06-10 00:00] Guard #2129 begins shift
[1518-09-13 00:56] wakes up
[1518-11-06 00:55] wakes up
[1518-04-25 00:03] Guard #733 begins shift
[1518-04-11 00:14] falls asleep
[1518-05-25 00:38] falls asleep
[1518-07-25 00:02] Guard #757 begins shift
[1518-10-03 00:40] falls asleep
[1518-07-31 23:58] Guard #733 begins shift
[1518-06-22 00:58] wakes up
[1518-05-14 00:19] falls asleep
[1518-08-08 23:46] Guard #1607 begins shift
[1518-07-24 00:19] wakes up
[1518-05-24 00:10] falls asleep
[1518-06-11 00:46] falls asleep
[1518-08-19 00:36] falls asleep
[1518-04-05 00:11] falls asleep
[1518-07-27 00:00] Guard #1297 begins shift
[1518-03-13 00:43] falls asleep
[1518-04-22 00:22] falls asleep
[1518-11-07 00:54] wakes up
[1518-03-31 00:00] Guard #2293 begins shift
[1518-10-28 00:12] falls asleep
[1518-03-30 00:32] falls asleep
[1518-03-23 00:49] wakes up
[1518-04-21 00:17] wakes up
[1518-06-25 00:52] falls asleep
[1518-10-23 00:45] wakes up
[1518-08-20 23:57] Guard #2819 begins shift
[1518-08-17 00:59] wakes up
[1518-10-05 00:45] falls asleep
[1518-03-22 00:45] falls asleep
[1518-03-25 00:12] falls asleep
[1518-11-11 00:16] wakes up
[1518-03-10 23:58] Guard #2129 begins shift
[1518-06-24 00:54] falls asleep
[1518-03-07 00:52] wakes up
[1518-10-01 00:58] wakes up
[1518-10-25 00:38] wakes up
[1518-09-01 00:31] falls asleep
[1518-08-02 00:02] falls asleep
[1518-04-26 00:52] falls asleep
[1518-03-30 00:12] falls asleep
[1518-09-30 00:04] falls asleep
[1518-04-24 00:04] Guard #2467 begins shift
[1518-03-05 23:53] Guard #1607 begins shift
[1518-03-02 23:59] Guard #1297 begins shift
[1518-03-02 00:00] Guard #733 begins shift
[1518-04-02 00:39] wakes up
[1518-04-04 00:06] falls asleep
[1518-09-10 23:56] Guard #2467 begins shift
[1518-10-17 23:56] Guard #1889 begins shift
[1518-11-02 00:44] falls asleep
[1518-10-27 00:59] wakes up
[1518-07-05 00:00] Guard #733 begins shift
[1518-07-03 00:33] wakes up
[1518-09-08 00:52] wakes up
[1518-08-06 00:01] Guard #349 begins shift
[1518-08-05 00:00] Guard #2467 begins shift
[1518-02-28 00:51] wakes up
[1518-05-12 00:55] wakes up
[1518-06-26 00:50] wakes up
[1518-08-11 00:51] wakes up
[1518-04-29 00:18] falls asleep
[1518-09-24 00:58] wakes up
[1518-03-28 00:42] wakes up
[1518-03-15 00:29] falls asleep
[1518-08-09 00:40] wakes up
[1518-04-11 23:59] Guard #227 begins shift
[1518-07-24 00:42] wakes up
[1518-04-29 00:58] wakes up
[1518-05-30 00:34] wakes up
[1518-09-29 00:33] wakes up
[1518-11-15 00:13] falls asleep
[1518-06-27 23:57] Guard #317 begins shift
[1518-06-22 00:24] falls asleep
[1518-06-19 00:00] Guard #1297 begins shift
[1518-07-15 00:48] wakes up
[1518-08-18 00:03] Guard #1933 begins shift
[1518-04-16 00:50] wakes up
[1518-09-01 00:59] wakes up
[1518-04-07 00:04] Guard #2129 begins shift
[1518-10-12 00:46] wakes up
[1518-08-27 00:54] falls asleep
[1518-09-21 00:05] falls asleep
[1518-08-19 00:22] wakes up
[1518-04-01 00:41] falls asleep
[1518-04-13 00:22] wakes up
[1518-10-02 00:49] wakes up
[1518-03-03 00:51] falls asleep
[1518-03-02 00:53] wakes up
[1518-06-24 23:56] Guard #1889 begins shift
[1518-11-19 00:19] falls asleep
[1518-09-25 00:08] falls asleep
[1518-05-10 00:25] falls asleep
[1518-03-22 00:47] wakes up
[1518-05-06 00:09] falls asleep
[1518-08-31 00:29] wakes up
[1518-10-23 00:39] falls asleep
[1518-06-08 00:57] falls asleep
[1518-11-08 00:38] falls asleep
[1518-05-16 00:26] wakes up
[1518-06-13 00:46] wakes up
[1518-10-25 00:31] falls asleep
[1518-09-12 00:47] wakes up
[1518-03-03 00:48] wakes up
[1518-03-02 00:25] wakes up
[1518-04-06 00:56] wakes up
[1518-02-24 00:55] falls asleep
[1518-05-30 00:45] falls asleep
[1518-09-24 00:21] falls asleep
[1518-09-07 00:27] falls asleep
[1518-03-05 00:53] falls asleep
[1518-03-24 00:06] falls asleep
[1518-06-07 23:56] Guard #757 begins shift
[1518-11-04 00:00] Guard #1297 begins shift
[1518-03-16 23:50] Guard #1933 begins shift
[1518-11-19 00:50] falls asleep
[1518-03-04 00:52] wakes up
[1518-05-21 23:50] Guard #2819 begins shift
[1518-11-11 00:43] falls asleep
[1518-10-05 00:54] wakes up
[1518-11-06 00:32] wakes up
[1518-06-22 23:57] Guard #2953 begins shift
[1518-06-20 23:48] Guard #751 begins shift
[1518-03-19 00:40] falls asleep
[1518-11-07 00:51] falls asleep
[1518-10-01 23:58] Guard #317 begins shift
[1518-06-23 00:18] falls asleep
[1518-06-17 00:02] Guard #1297 begins shift
[1518-08-14 00:32] falls asleep
[1518-09-22 00:16] wakes up
[1518-05-31 00:19] falls asleep
[1518-03-03 00:23] falls asleep
[1518-05-31 00:58] wakes up
[1518-09-12 00:13] falls asleep
[1518-10-08 00:31] falls asleep
[1518-11-18 00:54] wakes up
[1518-03-29 00:51] falls asleep
[1518-11-12 00:55] falls asleep
[1518-06-13 23:59] Guard #2819 begins shift
[1518-04-21 00:34] wakes up
[1518-05-27 00:46] falls asleep
[1518-07-24 00:37] wakes up
[1518-06-04 00:21] falls asleep
[1518-06-01 00:53] falls asleep
[1518-06-23 00:42] falls asleep
[1518-04-25 23:58] Guard #3433 begins shift
[1518-05-04 00:49] falls asleep
[1518-08-31 23:59] Guard #349 begins shift
[1518-03-13 23:47] Guard #1607 begins shift
[1518-06-23 00:26] wakes up
[1518-06-11 00:27] falls asleep
[1518-05-13 00:53] wakes up
[1518-08-02 00:23] wakes up
[1518-08-15 00:21] wakes up
[1518-06-12 23:57] Guard #3137 begins shift
[1518-04-21 00:32] falls asleep
[1518-04-19 00:26] falls asleep
[1518-11-04 00:47] wakes up
[1518-09-09 00:04] Guard #2129 begins shift
[1518-09-06 00:50] wakes up
[1518-10-06 00:16] wakes up
[1518-08-20 00:46] wakes up
[1518-03-31 00:57] wakes up
[1518-10-31 00:58] wakes up
[1518-08-12 00:16] falls asleep
[1518-08-23 00:54] falls asleep
[1518-05-25 00:47] wakes up
[1518-05-25 23:49] Guard #2953 begins shift
[1518-03-23 00:09] falls asleep
[1518-07-19 23:54] Guard #349 begins shift
[1518-04-26 00:44] wakes up
[1518-11-16 00:36] falls asleep
[1518-06-10 00:38] falls asleep
[1518-04-29 23:57] Guard #733 begins shift
[1518-08-18 00:38] wakes up
[1518-06-14 23:54] Guard #2819 begins shift
[1518-05-26 23:48] Guard #227 begins shift
[1518-08-14 00:42] wakes up
[1518-07-22 00:20] falls asleep
[1518-05-26 00:09] wakes up
[1518-05-08 00:56] wakes up
[1518-06-17 00:47] wakes up
[1518-05-27 00:57] wakes up
[1518-09-21 00:52] falls asleep
[1518-06-02 00:00] falls asleep
[1518-05-24 23:57] Guard #2467 begins shift
[1518-05-14 00:09] falls asleep
[1518-07-07 00:32] falls asleep
[1518-04-01 00:51] wakes up
[1518-05-19 23:58] Guard #349 begins shift
[1518-05-30 00:58] wakes up
[1518-10-26 00:02] falls asleep
[1518-07-21 00:39] falls asleep
[1518-08-15 00:08] falls asleep
[1518-06-11 00:00] Guard #733 begins shift
[1518-07-01 00:56] wakes up
[1518-09-28 00:01] Guard #1013 begins shift
[1518-11-12 00:12] falls asleep
[1518-10-20 00:42] wakes up
[1518-05-08 00:23] falls asleep
[1518-08-05 00:40] falls asleep
[1518-10-28 00:56] wakes up
[1518-09-14 00:46] wakes up
[1518-07-27 00:15] wakes up
[1518-06-16 00:42] wakes up
[1518-05-23 00:46] wakes up
[1518-09-20 00:55] wakes up
[1518-11-14 00:54] wakes up
[1518-09-04 00:05] falls asleep
[1518-06-30 00:36] wakes up
[1518-11-08 00:41] wakes up
[1518-10-03 00:04] Guard #2467 begins shift
[1518-09-20 00:01] falls asleep
[1518-06-26 23:56] Guard #733 begins shift
[1518-10-31 00:06] falls asleep
[1518-04-01 00:00] Guard #349 begins shift
[1518-07-21 00:45] falls asleep
[1518-07-25 23:50] Guard #1297 begins shift
[1518-09-22 00:00] Guard #1607 begins shift
[1518-08-29 00:51] falls asleep
[1518-06-08 00:35] wakes up
[1518-09-15 00:51] wakes up
[1518-09-08 00:20] falls asleep
[1518-11-04 00:11] falls asleep
[1518-08-20 00:35] falls asleep
[1518-09-22 00:39] falls asleep
[1518-05-23 00:01] Guard #733 begins shift
[1518-08-18 00:12] falls asleep
[1518-04-27 00:26] falls asleep
[1518-06-11 00:51] wakes up
[1518-07-16 00:58] wakes up
[1518-03-16 00:12] falls asleep
[1518-08-11 00:17] wakes up
[1518-04-11 00:28] wakes up
[1518-05-22 00:56] wakes up
[1518-06-22 00:05] falls asleep
[1518-02-27 00:52] wakes up
[1518-04-08 00:04] Guard #1297 begins shift
[1518-08-13 00:51] wakes up
[1518-04-28 23:58] Guard #733 begins shift
[1518-09-05 00:12] wakes up
[1518-07-13 00:00] Guard #2293 begins shift
[1518-09-20 00:27] wakes up
[1518-09-26 00:20] falls asleep
[1518-03-14 00:17] wakes up
[1518-07-18 00:13] falls asleep
[1518-10-18 00:41] wakes up
[1518-11-22 00:47] wakes up
[1518-06-15 23:57] Guard #349 begins shift
[1518-05-12 00:45] wakes up
[1518-07-30 00:42] falls asleep
[1518-11-05 23:46] Guard #2953 begins shift
[1518-06-29 23:58] Guard #2819 begins shift
[1518-05-10 00:58] wakes up
[1518-10-28 00:55] falls asleep
[1518-10-24 00:41] wakes up
[1518-03-28 00:34] falls asleep
[1518-07-02 00:28] falls asleep
[1518-09-17 00:21] wakes up
[1518-08-02 23:57] Guard #1889 begins shift
[1518-10-09 00:52] falls asleep
[1518-10-14 23:50] Guard #263 begins shift
[1518-03-28 23:57] Guard #2129 begins shift
[1518-10-15 00:36] falls asleep
[1518-04-27 00:49] wakes up
[1518-04-03 00:39] wakes up
[1518-04-21 00:46] falls asleep
[1518-05-06 00:01] Guard #1607 begins shift
[1518-04-09 00:02] Guard #3433 begins shift
[1518-11-12 00:51] wakes up
[1518-05-23 00:58] wakes up
[1518-11-06 00:51] falls asleep
[1518-10-16 00:54] falls asleep
[1518-10-27 00:55] falls asleep
[1518-07-01 23:58] Guard #317 begins shift
[1518-10-01 00:31] falls asleep
[1518-07-29 00:00] Guard #2467 begins shift
[1518-02-24 00:29] falls asleep
[1518-07-21 00:41] wakes up
[1518-07-19 00:13] falls asleep
[1518-07-27 00:14] falls asleep
[1518-09-18 00:00] Guard #2819 begins shift
[1518-07-05 23:54] Guard #2129 begins shift
[1518-06-14 00:57] wakes up
[1518-03-05 00:59] wakes up
[1518-03-02 00:16] falls asleep
[1518-08-29 00:48] wakes up
[1518-11-07 00:58] wakes up
[1518-11-11 00:11] falls asleep
[1518-04-06 00:46] wakes up
[1518-06-02 00:13] wakes up
[1518-09-29 00:02] Guard #3433 begins shift
[1518-09-21 00:17] wakes up
[1518-05-12 00:24] falls asleep
[1518-05-19 00:02] Guard #1933 begins shift
[1518-06-02 23:56] Guard #349 begins shift
[1518-03-12 00:54] wakes up
[1518-07-22 00:54] wakes up
[1518-09-17 00:04] Guard #1607 begins shift
[1518-06-05 00:03] Guard #1999 begins shift
[1518-04-06 00:00] Guard #2129 begins shift
[1518-05-19 00:34] wakes up
[1518-07-12 00:14] falls asleep
[1518-10-04 00:21] falls asleep
[1518-06-20 00:13] falls asleep
[1518-08-24 00:41] falls asleep
[1518-02-23 00:21] falls asleep
[1518-11-22 00:37] wakes up
[1518-07-27 00:50] wakes up
[1518-09-27 00:20] falls asleep
[1518-08-23 23:56] Guard #1999 begins shift
[1518-06-08 00:41] falls asleep
[1518-07-01 00:44] wakes up
[1518-08-07 00:09] falls asleep
[1518-11-09 00:30] wakes up
[1518-10-13 23:50] Guard #2953 begins shift
[1518-04-02 00:46] falls asleep
[1518-02-27 00:29] falls asleep
[1518-07-21 00:59] wakes up
[1518-08-04 00:35] falls asleep
[1518-11-19 00:23] wakes up
[1518-09-26 23:56] Guard #349 begins shift
[1518-10-26 00:10] wakes up
[1518-05-03 00:01] Guard #1999 begins shift
[1518-09-23 00:33] wakes up
[1518-03-30 00:19] wakes up
[1518-07-26 00:00] falls asleep
[1518-07-30 00:39] wakes up
[1518-04-18 00:48] falls asleep
[1518-07-20 00:33] wakes up
[1518-06-12 00:09] falls asleep
[1518-09-07 00:57] falls asleep
[1518-08-09 00:01] wakes up
[1518-07-10 00:51] wakes up
[1518-05-02 00:11] falls asleep
[1518-09-10 00:54] wakes up
[1518-05-13 23:59] Guard #263 begins shift
[1518-06-25 23:57] Guard #263 begins shift
[1518-05-21 00:59] wakes up
[1518-04-14 00:06] falls asleep
[1518-10-19 00:07] falls asleep
[1518-07-14 23:49] Guard #3137 begins shift
[1518-11-14 00:30] falls asleep
[1518-07-22 23:58] Guard #1999 begins shift
[1518-11-19 00:39] wakes up
[1518-05-21 00:57] falls asleep
[1518-03-07 00:29] falls asleep
[1518-08-12 00:57] wakes up
[1518-05-28 00:41] wakes up
[1518-03-20 00:26] falls asleep
[1518-08-19 23:59] Guard #2467 begins shift
[1518-10-05 00:16] falls asleep
[1518-05-17 23:58] Guard #619 begins shift
[1518-04-18 00:56] wakes up
[1518-10-07 00:11] falls asleep
[1518-06-01 00:59] wakes up
[1518-09-24 23:58] Guard #2953 begins shift
[1518-04-30 00:39] falls asleep
[1518-05-30 00:22] falls asleep
[1518-05-12 00:25] wakes up
[1518-03-11 00:54] wakes up
[1518-07-25 00:50] falls asleep
[1518-10-16 00:56] wakes up
[1518-06-18 00:37] wakes up
[1518-09-13 00:39] falls asleep
[1518-08-22 00:22] falls asleep
[1518-05-15 00:30] falls asleep
[1518-09-17 00:06] falls asleep
[1518-08-22 00:46] falls asleep
[1518-09-16 00:43] falls asleep
[1518-10-06 23:57] Guard #1889 begins shift
[1518-03-03 00:36] wakes up
[1518-11-01 00:27] falls asleep
[1518-07-13 00:52] falls asleep
[1518-04-14 00:52] wakes up
[1518-11-21 00:59] wakes up
[1518-10-15 00:33] wakes up
[1518-10-12 00:38] falls asleep
[1518-07-16 00:53] falls asleep
[1518-11-18 00:49] falls asleep
[1518-11-06 00:45] wakes up
[1518-09-18 00:24] falls asleep
[1518-10-19 23:52] Guard #757 begins shift
[1518-07-20 00:00] falls asleep
[1518-07-02 00:12] falls asleep
[1518-07-03 00:52] wakes up
[1518-04-13 00:31] falls asleep
[1518-03-24 00:00] Guard #751 begins shift
[1518-05-14 00:11] wakes up
[1518-04-21 23:57] Guard #1889 begins shift
[1518-10-08 00:54] falls asleep
[1518-05-09 00:43] wakes up
[1518-03-01 00:59] wakes up
[1518-03-16 00:54] wakes up
[1518-10-20 00:49] wakes up
[1518-02-23 00:29] wakes up
[1518-08-26 00:57] wakes up
[1518-10-18 00:24] falls asleep
[1518-05-20 23:56] Guard #1607 begins shift
[1518-09-15 00:43] falls asleep
[1518-10-04 23:59] Guard #1019 begins shift
[1518-11-03 00:01] Guard #151 begins shift
[1518-03-18 00:38] wakes up
[1518-10-24 00:17] falls asleep
[1518-06-29 00:42] wakes up
[1518-08-01 23:46] Guard #349 begins shift
[1518-05-06 00:59] wakes up
[1518-04-16 00:10] falls asleep
[1518-10-25 00:15] falls asleep
[1518-04-24 00:37] falls asleep
[1518-09-05 00:49] falls asleep
[1518-06-19 00:53] wakes up
[1518-07-23 00:22] falls asleep
[1518-10-10 00:11] wakes up
[1518-04-08 00:36] falls asleep
[1518-03-31 00:52] falls asleep
[1518-10-05 00:30] falls asleep
[1518-11-23 00:55] wakes up
[1518-11-05 00:49] wakes up
[1518-11-12 00:58] wakes up
[1518-07-31 00:22] wakes up
[1518-09-04 00:17] wakes up
[1518-08-22 00:03] Guard #1019 begins shift
[1518-06-29 00:02] Guard #1999 begins shift
[1518-05-07 23:58] Guard #2467 begins shift
[1518-03-11 00:12] falls asleep
[1518-08-25 00:15] falls asleep
[1518-05-09 00:04] Guard #3433 begins shift
[1518-09-19 00:46] falls asleep
[1518-06-24 00:48] wakes up
[1518-08-22 00:34] wakes up
[1518-03-07 00:00] Guard #1999 begins shift
[1518-05-15 23:50] Guard #751 begins shift
[1518-10-22 00:08] falls asleep
[1518-07-08 00:07] falls asleep
[1518-05-22 00:48] falls asleep
[1518-09-06 00:23] falls asleep
[1518-06-21 00:37] falls asleep
[1518-05-02 00:43] wakes up
[1518-09-22 00:13] falls asleep
[1518-11-12 00:02] Guard #3433 begins shift
[1518-09-17 00:46] wakes up
[1518-08-12 23:50] Guard #1297 begins shift
[1518-06-11 00:55] wakes up
[1518-06-01 00:35] falls asleep
[1518-04-14 00:44] wakes up
[1518-07-09 00:02] Guard #733 begins shift
[1518-08-31 00:54] falls asleep
[1518-05-24 00:20] wakes up
[1518-10-25 23:46] Guard #2129 begins shift
[1518-07-20 00:48] falls asleep
[1518-05-13 00:51] falls asleep
[1518-03-21 23:58] Guard #2129 begins shift
[1518-03-24 23:56] Guard #751 begins shift
[1518-10-18 00:25] wakes up
[1518-06-04 00:03] Guard #1999 begins shift
[1518-08-21 00:50] wakes up
[1518-10-30 00:33] wakes up
[1518-05-19 00:07] falls asleep
[1518-10-03 23:59] Guard #349 begins shift
[1518-09-19 00:00] Guard #1019 begins shift
[1518-08-01 00:16] wakes up
[1518-05-03 00:24] falls asleep
[1518-08-07 00:30] wakes up
[1518-07-17 00:02] Guard #1889 begins shift
[1518-05-19 00:15] wakes up
[1518-10-09 00:04] Guard #2293 begins shift
[1518-06-20 00:01] Guard #733 begins shift
[1518-08-06 00:56] wakes up
[1518-03-01 00:47] falls asleep
[1518-10-10 23:59] Guard #1013 begins shift
[1518-03-20 00:00] Guard #1999 begins shift
[1518-03-31 00:40] falls asleep
[1518-03-07 23:57] Guard #263 begins shift
[1518-09-27 00:51] wakes up
[1518-11-19 00:00] Guard #317 begins shift
[1518-10-25 00:43] falls asleep
[1518-09-14 00:03] Guard #3433 begins shift
[1518-08-04 00:56] wakes up
[1518-05-09 00:55] wakes up
[1518-09-20 00:45] falls asleep
[1518-05-23 00:54] falls asleep
[1518-02-23 00:48] wakes up
[1518-08-27 00:59] wakes up
[1518-09-23 00:02] Guard #227 begins shift
[1518-03-24 00:34] falls asleep
[1518-07-22 00:52] falls asleep
[1518-04-08 00:49] wakes up
[1518-03-06 00:49] wakes up
[1518-11-08 00:00] Guard #3433 begins shift
[1518-05-26 00:01] falls asleep
[1518-10-16 23:54] Guard #751 begins shift
[1518-03-10 00:13] wakes up
[1518-02-27 23:58] Guard #1297 begins shift
[1518-05-17 00:02] falls asleep
[1518-11-18 00:03] Guard #1999 begins shift
[1518-07-02 23:48] Guard #227 begins shift
[1518-04-25 00:43] wakes up
[1518-08-31 00:08] wakes up
[1518-07-17 00:35] falls asleep
[1518-07-23 00:41] wakes up
[1518-11-14 00:14] wakes up
[1518-05-17 00:47] wakes up
[1518-08-09 00:14] falls asleep
[1518-09-16 00:31] wakes up
[1518-10-22 23:59] Guard #1607 begins shift
[1518-05-29 23:59] Guard #2953 begins shift
[1518-11-21 23:59] Guard #3137 begins shift
[1518-11-07 00:18] falls asleep
[1518-05-28 00:44] falls asleep
[1518-07-29 23:58] Guard #1607 begins shift
[1518-11-02 00:45] wakes up
[1518-03-05 00:48] wakes up
[1518-08-31 00:19] falls asleep
[1518-06-20 00:58] wakes up
[1518-03-20 23:59] Guard #151 begins shift
[1518-03-30 00:54] wakes up
[1518-10-26 00:24] falls asleep
[1518-11-22 00:55] wakes up
[1518-10-10 00:07] falls asleep
[1518-07-30 23:59] Guard #751 begins shift
[1518-11-23 00:24] falls asleep
[1518-08-15 00:00] Guard #751 begins shift
[1518-07-07 00:55] wakes up
[1518-07-10 00:22] falls asleep
[1518-06-27 00:07] falls asleep
[1518-11-22 00:35] falls asleep
[1518-04-15 00:50] wakes up
[1518-10-26 00:31] wakes up
[1518-06-01 00:46] wakes up
[1518-10-16 00:01] Guard #1607 begins shift
[1518-03-22 23:58] Guard #751 begins shift
[1518-09-15 00:03] Guard #3433 begins shift
[1518-04-06 00:13] falls asleep
[1518-04-26 00:55] wakes up
[1518-08-03 23:58] Guard #2819 begins shift
[1518-05-12 00:44] falls asleep
[1518-10-18 00:35] falls asleep
[1518-07-06 00:03] falls asleep
[1518-06-08 00:58] wakes up
[1518-10-09 23:58] Guard #2129 begins shift
[1518-08-01 00:14] falls asleep
[1518-03-22 00:54] falls asleep
[1518-06-17 00:40] falls asleep
[1518-05-09 00:47] falls asleep
[1518-10-21 00:55] wakes up
[1518-07-05 00:17] falls asleep
[1518-07-08 00:38] wakes up
[1518-09-30 00:48] wakes up
[1518-07-03 00:00] falls asleep
[1518-05-03 00:49] wakes up
[1518-04-19 00:52] wakes up
[1518-07-12 00:35] falls asleep
[1518-06-03 00:24] falls asleep
[1518-11-13 00:33] wakes up
[1518-08-23 00:00] Guard #263 begins shift
[1518-08-26 00:01] Guard #2819 begins shift
[1518-09-09 00:41] wakes up
[1518-10-28 00:31] wakes up
[1518-04-12 00:56] wakes up
[1518-07-27 23:54] Guard #2953 begins shift
[1518-11-11 00:44] wakes up
[1518-07-22 00:48] wakes up
[1518-10-13 00:53] falls asleep
[1518-03-12 00:00] Guard #2953 begins shift
[1518-03-04 00:41] falls asleep
[1518-04-27 00:28] wakes up
[1518-06-06 00:20] falls asleep
[1518-07-13 00:38] falls asleep
[1518-09-17 00:35] falls asleep
[1518-03-24 00:42] wakes up
[1518-05-15 00:53] wakes up
[1518-03-26 00:00] Guard #619 begins shift
[1518-06-30 00:21] falls asleep
[1518-09-26 00:59] wakes up
[1518-08-16 00:22] falls asleep
[1518-03-05 00:47] falls asleep
[1518-06-19 00:32] falls asleep
[1518-03-13 00:04] Guard #2467 begins shift
[1518-11-09 00:33] falls asleep
[1518-09-19 23:52] Guard #757 begins shift
[1518-02-24 00:35] wakes up
[1518-05-11 00:19] falls asleep
[1518-04-06 00:17] wakes up
[1518-10-30 00:01] falls asleep
[1518-08-08 00:30] falls asleep
[1518-03-17 00:05] falls asleep
[1518-11-06 00:02] falls asleep
[1518-04-29 00:23] wakes up
[1518-08-09 23:52] Guard #751 begins shift
[1518-05-05 00:02] Guard #227 begins shift
[1518-11-16 00:58] wakes up
[1518-11-15 00:00] Guard #1999 begins shift
[1518-09-05 00:00] falls asleep
[1518-11-07 00:04] Guard #1889 begins shift
[1518-08-25 00:39] falls asleep
[1518-07-03 00:41] falls asleep
[1518-05-01 23:56] Guard #2953 begins shift
[1518-07-01 00:51] falls asleep
[1518-04-05 00:50] wakes up
[1518-08-09 00:00] falls asleep
[1518-04-14 00:01] Guard #1889 begins shift
[1518-10-06 00:21] falls asleep
[1518-04-04 23:56] Guard #1297 begins shift
[1518-02-23 00:03] Guard #751 begins shift
[1518-09-24 00:03] Guard #2467 begins shift
[1518-09-16 00:00] Guard #1019 begins shift
[1518-11-18 00:09] falls asleep
[1518-06-08 00:54] wakes up
[1518-08-28 00:26] falls asleep
[1518-10-21 23:56] Guard #3433 begins shift
[1518-08-01 00:57] wakes up
[1518-05-05 00:59] wakes up
[1518-07-03 23:57] Guard #3137 begins shift
[1518-08-26 23:59] Guard #2467 begins shift
[1518-10-31 00:03] Guard #317 begins shift
[1518-03-08 00:15] falls asleep
[1518-05-06 00:54] falls asleep
[1518-06-02 00:47] falls asleep
[1518-04-14 00:23] falls asleep
[1518-07-13 00:42] wakes up
[1518-07-02 00:25] wakes up
[1518-03-12 00:49] falls asleep
[1518-07-12 00:23] wakes up
[1518-03-14 00:02] falls asleep
[1518-09-03 00:02] Guard #2129 begins shift
[1518-06-25 00:48] wakes up
[1518-04-20 00:53] wakes up
[1518-02-25 00:21] falls asleep
[1518-06-25 00:54] wakes up
[1518-06-24 00:00] Guard #2293 begins shift
[1518-07-06 23:56] Guard #1297 begins shift
[1518-08-09 00:55] wakes up
[1518-06-18 00:02] Guard #1889 begins shift
[1518-08-11 00:16] falls asleep
[1518-09-21 00:54] wakes up
[1518-06-28 00:54] wakes up
[1518-06-16 00:12] falls asleep
[1518-03-03 00:54] wakes up
[1518-05-25 00:58] wakes up
[1518-07-19 00:18] wakes up
[1518-06-12 00:00] Guard #2467 begins shift
[1518-08-31 00:01] falls asleep
[1518-10-28 00:19] wakes up
[1518-03-19 00:53] wakes up
[1518-07-28 00:01] falls asleep
[1518-11-18 00:43] wakes up
[1518-08-10 00:27] wakes up
[1518-11-16 23:57] Guard #1013 begins shift
[1518-04-02 00:27] falls asleep
[1518-10-17 00:35] wakes up
[1518-03-15 00:06] falls asleep
[1518-11-20 23:57] Guard #2953 begins shift
[1518-08-08 00:02] Guard #317 begins shift
[1518-05-23 23:57] Guard #3433 begins shift
[1518-11-02 00:39] wakes up
[1518-04-24 00:41] wakes up
[1518-08-24 00:42] wakes up
[1518-07-24 00:03] Guard #751 begins shift
[1518-10-27 23:58] Guard #2293 begins shift
[1518-06-05 00:55] wakes up
[1518-05-20 00:26] wakes up
[1518-04-07 00:49] wakes up
[1518-09-07 00:01] Guard #3433 begins shift
[1518-10-05 23:53] Guard #751 begins shift
[1518-10-20 00:46] falls asleep
[1518-10-02 00:36] falls asleep
[1518-08-19 00:52] wakes up
[1518-03-27 00:54] wakes up
[1518-07-30 00:38] falls asleep
[1518-07-28 00:07] wakes up
[1518-05-01 00:02] Guard #2293 begins shift
[1518-07-16 00:01] Guard #2293 begins shift
[1518-06-24 00:11] falls asleep
[1518-05-11 00:29] wakes up
[1518-10-31 23:56] Guard #1889 begins shift
[1518-05-15 00:56] falls asleep
[1518-08-28 00:00] Guard #2129 begins shift
[1518-08-17 00:50] falls asleep
[1518-07-19 00:55] falls asleep
[1518-04-09 00:14] falls asleep
[1518-08-09 00:46] falls asleep
[1518-03-13 00:23] wakes up
[1518-06-23 00:54] wakes up
[1518-08-31 00:58] wakes up
[1518-04-28 00:00] Guard #1019 begins shift
[1518-03-15 00:07] wakes up
[1518-08-15 23:56] Guard #2467 begins shift
[1518-03-06 00:03] falls asleep
[1518-09-09 00:07] falls asleep
[1518-10-03 00:36] wakes up
[1518-09-12 00:01] Guard #751 begins shift
[1518-09-18 00:58] wakes up
[1518-05-19 00:25] falls asleep
[1518-03-15 00:48] wakes up
[1518-07-15 00:16] wakes up
[1518-07-15 00:39] falls asleep
[1518-06-27 00:21] wakes up
[1518-05-28 00:20] falls asleep
[1518-06-06 00:28] wakes up
[1518-09-05 23:57] Guard #1889 begins shift
[1518-04-15 00:43] wakes up
[1518-05-12 00:51] falls asleep
[1518-09-19 00:47] wakes up
[1518-10-13 00:00] Guard #1999 begins shift
[1518-04-15 23:59] Guard #3433 begins shift
[1518-10-27 00:37] falls asleep
[1518-03-09 00:33] falls asleep
[1518-11-22 00:41] falls asleep
[1518-04-02 00:02] Guard #317 begins shift
[1518-02-25 00:00] Guard #3137 begins shift
[1518-08-29 23:59] Guard #2293 begins shift
[1518-11-01 23:58] Guard #1019 begins shift
[1518-07-20 00:58] wakes up
[1518-08-30 00:21] falls asleep
[1518-03-07 00:35] wakes up
[1518-09-18 00:56] falls asleep
[1518-07-10 23:56] Guard #151 begins shift
[1518-03-15 00:04] Guard #317 begins shift
[1518-04-28 00:11] falls asleep
[1518-02-28 00:37] falls asleep
[1518-10-03 00:51] wakes up
[1518-10-29 00:43] wakes up
[1518-10-10 00:52] wakes up
[1518-02-23 00:46] falls asleep
[1518-05-16 00:05] falls asleep
[1518-03-17 23:56] Guard #3433 begins shift
[1518-02-24 00:48] falls asleep
[1518-06-22 00:13] wakes up
[1518-05-28 00:53] falls asleep
[1518-07-15 00:28] falls asleep
[1518-11-09 00:00] Guard #1999 begins shift
[1518-10-27 00:00] Guard #349 begins shift
[1518-04-21 00:05] falls asleep
[1518-04-14 00:51] falls asleep
[1518-05-06 00:47] wakes up
[1518-10-05 00:26] wakes up
[1518-06-15 00:03] falls asleep
[1518-05-07 00:51] wakes up
[1518-03-10 00:19] falls asleep
[1518-05-11 23:58] Guard #2819 begins shift
[1518-05-10 00:47] falls asleep
[1518-07-06 00:52] wakes up
[1518-11-21 00:19] falls asleep
[1518-10-08 00:44] wakes up
[1518-05-15 00:59] wakes up
[1518-09-09 23:51] Guard #1933 begins shift
[1518-10-16 00:40] wakes up
[1518-06-28 00:45] falls asleep
[1518-08-24 23:56] Guard #757 begins shift
[1518-04-13 00:42] wakes up
[1518-03-12 00:57] falls asleep
[1518-11-20 00:55] falls asleep
[1518-05-22 00:45] wakes up
[1518-02-25 00:45] wakes up
[1518-05-28 00:01] Guard #757 begins shift
[1518-07-09 00:26] falls asleep
[1518-11-05 00:01] Guard #263 begins shift
[1518-10-24 23:59] Guard #1607 begins shift
[1518-08-18 23:49] Guard #2129 begins shift
[1518-03-05 00:00] Guard #3433 begins shift
[1518-04-19 00:00] Guard #1297 begins shift
[1518-11-11 00:00] Guard #751 begins shift
[1518-07-29 00:17] falls asleep
[1518-03-20 00:48] wakes up
[1518-08-06 00:08] falls asleep
[1518-10-17 00:23] falls asleep
[1518-10-04 00:59] wakes up
[1518-09-13 00:17] wakes up
[1518-07-25 00:57] wakes up
[1518-04-06 00:35] falls asleep
[1518-09-11 00:18] falls asleep
[1518-06-17 00:51] falls asleep
[1518-04-27 00:36] falls asleep
[1518-09-16 00:09] falls asleep
[1518-10-06 00:43] wakes up
[1518-07-12 00:00] Guard #227 begins shift
[1518-06-25 00:44] falls asleep
[1518-08-24 00:23] falls asleep
[1518-11-20 00:52] wakes up
[1518-04-25 00:15] falls asleep
[1518-05-23 00:27] falls asleep
[1518-06-13 00:40] falls asleep
[1518-05-07 00:02] Guard #757 begins shift
[1518-09-03 23:50] Guard #1607 begins shift
[1518-08-25 00:49] wakes up
[1518-08-30 23:47] Guard #3137 begins shift
[1518-05-10 00:12] wakes up
[1518-08-17 00:23] falls asleep
[1518-10-17 00:00] falls asleep
[1518-06-03 00:25] wakes up
[1518-03-10 00:11] falls asleep
[1518-09-10 00:02] falls asleep
[1518-09-25 00:49] wakes up
[1518-09-18 00:51] wakes up
[1518-07-13 23:56] Guard #2467 begins shift
[1518-11-19 00:59] wakes up
[1518-11-04 00:46] falls asleep
[1518-03-03 23:58] Guard #1889 begins shift
[1518-08-17 00:27] wakes up
[1518-09-04 00:52] wakes up
[1518-06-09 00:03] Guard #1013 begins shift
[1518-03-12 00:59] wakes up
[1518-11-15 23:57] Guard #1999 begins shift
[1518-10-11 23:59] Guard #1019 begins shift
[1518-07-19 00:56] wakes up
[1518-09-02 00:51] wakes up
[1518-07-18 23:56] Guard #751 begins shift
[1518-10-24 00:04] Guard #733 begins shift
[1518-08-26 00:39] falls asleep
[1518-10-29 00:02] Guard #733 begins shift
[1518-05-14 00:26] wakes up
[1518-10-15 00:02] falls asleep
[1518-06-24 00:57] wakes up
[1518-05-22 00:01] falls asleep
[1518-08-13 00:02] falls asleep
[1518-11-07 00:57] falls asleep
[1518-11-13 00:22] falls asleep
[1518-04-26 00:37] falls asleep
[1518-03-26 23:57] Guard #2819 begins shift
[1518-04-03 00:04] falls asleep
[1518-10-25 00:22] wakes up
[1518-05-25 00:55] falls asleep
[1518-06-06 00:01] Guard #1933 begins shift
[1518-03-17 00:59] wakes up
[1518-08-13 23:57] Guard #317 begins shift
[1518-06-02 00:54] wakes up
[1518-09-20 23:52] Guard #349 begins shift
[1518-11-14 00:09] falls asleep
[1518-06-21 00:54] wakes up
[1518-07-09 00:40] wakes up
[1518-06-21 23:48] Guard #349 begins shift
[1518-02-24 00:50] wakes up
[1518-05-10 00:06] falls asleep
[1518-10-07 00:46] wakes up
[1518-04-14 00:15] wakes up
[1518-09-22 00:50] wakes up
[1518-07-27 00:29] falls asleep
[1518-07-15 00:35] wakes up
[1518-02-23 00:10] falls asleep
[1518-09-02 00:06] falls asleep
[1518-05-09 23:59] Guard #757 begins shift
[1518-06-17 00:55] wakes up
[1518-06-08 00:31] falls asleep
[1518-10-15 00:28] falls asleep
[1518-08-21 00:28] falls asleep
[1518-08-30 00:54] wakes up
[1518-04-18 00:51] wakes up
[1518-04-01 00:55] falls asleep
[1518-09-04 00:30] falls asleep
[1518-09-23 00:13] falls asleep
[1518-04-27 00:00] Guard #2467 begins shift
[1518-04-23 00:00] Guard #1013 begins shift
[1518-07-15 00:03] falls asleep
[1518-05-29 00:40] falls asleep
[1518-11-22 00:53] falls asleep
[1518-06-12 00:49] wakes up
[1518-04-02 23:52] Guard #2467 begins shift
[1518-10-13 00:57] wakes up
[1518-07-08 00:04] Guard #349 begins shift
[1518-08-11 00:01] Guard #1933 begins shift
[1518-10-06 00:47] falls asleep
[1518-02-26 00:44] falls asleep
[1518-05-27 00:17] wakes up
[1518-10-21 00:15] falls asleep
[1518-11-10 00:55] wakes up
[1518-05-20 00:19] falls asleep
[1518-08-10 00:00] falls asleep
[1518-05-16 23:52] Guard #751 begins shift
[1518-11-12 23:57] Guard #1889 begins shift
[1518-11-11 00:51] wakes up
[1518-04-18 00:19] falls asleep
[1518-08-16 23:57] Guard #349 begins shift
[1518-05-27 00:01] falls asleep
[1518-09-07 00:50] wakes up
[1518-07-17 23:48] Guard #2129 begins shift
[1518-04-02 00:59] wakes up
[1518-10-20 00:05] falls asleep
[1518-10-18 23:57] Guard #317 begins shift
[1518-08-11 00:40] falls asleep
[1518-11-13 23:58] Guard #2293 begins shift
[1518-07-24 00:35] falls asleep
[1518-04-07 00:17] falls asleep
[1518-05-13 00:00] Guard #2819 begins shift
[1518-04-13 00:20] falls asleep
[1518-06-07 00:00] Guard #2293 begins shift
[1518-11-19 00:29] falls asleep
[1518-03-19 00:04] Guard #733 begins shift
[1518-11-15 00:30] wakes up
[1518-11-22 23:58] Guard #1999 begins shift
[1518-02-24 00:02] Guard #1297 begins shift
[1518-06-10 00:54] wakes up
[1518-07-01 00:02] Guard #733 begins shift
[1518-10-06 00:53] wakes up
[1518-08-11 23:56] Guard #1019 begins shift
[1518-09-03 00:43] wakes up
[1518-04-04 00:03] Guard #2467 begins shift
[1518-03-03 00:41] falls asleep
[1518-07-12 00:48] wakes up
[1518-06-27 00:31] wakes up
[1518-08-28 00:56] wakes up
[1518-08-19 00:04] falls asleep
[1518-04-28 00:45] wakes up
[1518-02-26 00:57] wakes up
[1518-05-28 00:55] wakes up
[1518-09-07 00:58] wakes up
[1518-07-18 00:09] wakes up
[1518-04-30 00:52] wakes up
[1518-08-25 00:32] wakes up
[1518-04-18 00:20] wakes up
[1518-03-28 00:00] Guard #2467 begins shift
[1518-04-15 00:46] falls asleep
[1518-05-10 23:59] Guard #227 begins shift
[1518-10-28 00:30] falls asleep
[1518-06-01 23:46] Guard #2293 begins shift
[1518-07-04 00:46] wakes up
[1518-05-05 00:54] falls asleep
[1518-09-29 23:50] Guard #757 begins shift
[1518-07-30 00:54] wakes up
[1518-08-31 00:45] falls asleep
[1518-06-16 00:50] falls asleep
[1518-10-19 00:45] wakes up
[1518-09-12 23:58] Guard #751 begins shift
[1518-05-28 00:50] wakes up
[1518-09-05 00:51] wakes up
[1518-07-14 00:14] falls asleep
[1518-07-04 00:07] falls asleep
[1518-10-26 00:49] falls asleep
[1518-09-23 00:47] falls asleep
[1518-06-16 00:56] wakes up
[1518-03-16 00:02] Guard #1889 begins shift
[1518-10-15 00:50] wakes up
[1518-07-31 00:18] falls asleep
[1518-07-14 00:24] wakes up
[1518-05-29 00:53] wakes up
[1518-02-28 00:49] falls asleep
[1518-05-29 00:01] Guard #2953 begins shift
[1518-07-26 00:05] wakes up
[1518-07-22 00:04] Guard #263 begins shift
[1518-10-06 00:04] falls asleep
[1518-10-27 00:51] wakes up
[1518-10-25 00:58] wakes up
[1518-09-23 00:51] wakes up
[1518-07-17 00:48] wakes up
[1518-06-07 00:56] wakes up
[1518-03-08 23:58] Guard #1019 begins shift
[1518-03-25 00:59] wakes up
[1518-11-19 23:59] Guard #2293 begins shift
[1518-03-29 23:58] Guard #3433 begins shift
[1518-04-21 00:54] wakes up
[1518-09-16 00:56] wakes up
[1518-03-13 00:56] wakes up
[1518-07-18 00:01] falls asleep
[1518-07-10 00:00] Guard #1297 begins shift
[1518-09-03 00:42] falls asleep
[1518-06-27 00:25] falls asleep
[1518-10-08 00:58] wakes up
[1518-11-06 00:41] falls asleep
[1518-02-28 00:44] wakes up
[1518-03-23 00:45] falls asleep
[1518-03-23 00:39] wakes up
[1518-08-05 00:55] wakes up
[1518-09-04 23:50] Guard #2819 begins shift
[1518-04-18 00:55] falls asleep
[1518-10-10 00:36] falls asleep
[1518-10-29 00:14] falls asleep
[1518-08-31 00:47] wakes up
[1518-10-16 00:36] falls asleep
[1518-11-05 00:14] falls asleep
[1518-04-20 00:23] falls asleep
[1518-07-24 00:06] falls asleep
[1518-04-15 00:04] Guard #1297 begins shift
[1518-04-19 23:58] Guard #2467 begins shift
[1518-06-01 00:00] Guard #1607 begins shift
[1518-03-27 00:48] falls asleep
[1518-05-10 00:32] wakes up
[1518-06-06 00:47] falls asleep
[1518-08-03 00:28] falls asleep
[1518-02-24 00:58] wakes up
[1518-10-29 23:54] Guard #349 begins shift
[1518-10-02 00:19] falls asleep
[1518-09-07 23:59] Guard #1933 begins shift
[1518-06-29 00:50] falls asleep
[1518-08-29 00:57] wakes up
[1518-11-11 00:49] falls asleep
[1518-02-27 00:03] Guard #2293 begins shift
[1518-03-01 00:01] Guard #757 begins shift
[1518-04-29 00:32] falls asleep
[1518-06-05 00:37] falls asleep
[1518-03-29 00:57] wakes up
[1518-09-25 23:59] Guard #757 begins shift
[1518-06-11 00:54] falls asleep
[1518-03-23 00:14] wakes up
[1518-04-10 00:04] Guard #317 begins shift
[1518-03-02 00:30] falls asleep
[1518-11-20 00:59] wakes up
[1518-03-18 00:51] wakes up
[1518-06-26 00:32] falls asleep
[1518-04-10 00:10] falls asleep
[1518-08-22 00:51] wakes up
[1518-10-08 00:04] Guard #733 begins shift
[1518-06-30 00:49] wakes up
[1518-10-15 00:24] wakes up
[1518-06-07 00:43] falls asleep
[1518-11-20 00:51] falls asleep
[1518-05-31 00:04] Guard #317 begins shift
[1518-03-31 00:41] wakes up
[1518-06-21 00:04] falls asleep
[1518-07-02 00:44] wakes up
[1518-11-09 00:43] wakes up
[1518-03-09 23:56] Guard #2467 begins shift
[1518-04-13 00:04] Guard #3433 begins shift
[1518-09-11 00:47] wakes up
[1518-09-14 00:44] falls asleep
[1518-07-21 00:00] Guard #1297 begins shift
[1518-04-04 00:55] wakes up
[1518-11-04 00:24] wakes up
[1518-08-03 00:32] wakes up
[1518-04-09 00:45] wakes up
[1518-10-02 00:26] wakes up
[1518-02-23 00:15] wakes up
[1518-11-01 00:55] wakes up
[1518-06-21 00:18] wakes up
[1518-10-28 00:41] falls asleep
[1518-07-01 00:09] falls asleep
[1518-02-25 23:57] Guard #2293 begins shift
[1518-11-09 00:22] falls asleep
[1518-04-17 00:00] Guard #151 begins shift
[1518-06-29 00:41] falls asleep
[1518-08-07 00:04] Guard #1889 begins shift
[1518-07-05 00:35] wakes up
[1518-07-24 00:40] falls asleep
[1518-11-10 00:00] Guard #2953 begins shift
[1518-04-20 23:51] Guard #733 begins shift
[1518-08-01 00:23] falls asleep
[1518-03-18 00:17] falls asleep
[1518-04-10 00:59] wakes up
[1518-06-29 00:54] wakes up
[1518-03-18 00:41] falls asleep
[1518-05-15 00:00] Guard #1889 begins shift
[1518-10-03 00:09] falls asleep
[1518-10-17 00:20] wakes up
[1518-03-24 00:29] wakes up
[1518-07-29 00:37] wakes up
[1518-11-07 00:44] wakes up
[1518-06-04 00:39] wakes up
[1518-10-22 00:45] wakes up
[1518-10-26 00:56] wakes up
[1518-04-18 00:00] Guard #2467 begins shift
[1518-10-05 00:40] wakes up
[1518-07-13 00:56] wakes up
[1518-06-06 00:50] wakes up
[1518-08-23 00:56] wakes up
[1518-04-11 00:00] Guard #263 begins shift
[1518-05-07 00:44] falls asleep
[1518-03-07 00:41] falls asleep
[1518-10-28 00:51] wakes up
[1518-08-29 00:28] falls asleep
[1518-04-06 00:52] falls asleep
[1518-10-14 00:28] wakes up
[1518-04-01 00:57] wakes up
[1518-06-11 00:41] wakes up
[1518-09-29 00:22] falls asleep
[1518-08-29 00:00] Guard #3433 begins shift
[1518-10-21 00:03] Guard #1999 begins shift
[1518-04-12 00:24] falls asleep
[1518-10-09 00:56] wakes up
[1518-05-04 00:59] wakes up
[1518-05-01 00:50] falls asleep
[1518-06-18 00:07] falls asleep
[1518-03-08 00:44] wakes up
[1518-06-14 00:42] falls asleep
[1518-10-01 00:02] Guard #1297 begins shift
[1518-03-13 00:15] falls asleep
[1518-05-04 00:04] Guard #3433 begins shift
[1518-05-09 00:37] falls asleep
[1518-08-08 00:55] wakes up
[1518-08-16 00:45] wakes up
[1518-03-09 00:35] wakes up
[1518-07-18 00:45] wakes up
[1518-04-15 00:09] falls asleep
[1518-08-24 00:35] wakes up
[1518-03-22 00:58] wakes up
[1518-06-30 00:39] falls asleep
[1518-11-02 00:09] falls asleep
[1518-11-10 00:33] falls asleep
"
