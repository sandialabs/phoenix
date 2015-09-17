package gov.sandia.phoenix.time

import org.scalatest.FunSuite

class IntervalTest extends FunSuite {
  test("Contains Interval"){
    val t0 = TimeBuilder(2006, 1, 1, 0, 0, 0, 0)
    val t1 = TimeBuilder(2006, 1, 2, 0, 0, 0, 0)
    val t2 = TimeBuilder(2006, 1, 3, 0, 0, 0, 0)
    val t3 = TimeBuilder(2006, 1, 4, 0, 0, 0, 0)

    //3 abutting intervals
    val interval0 = Interval(t0, t1)
    val interval1 = Interval(t1, t2)
    val interval2 = Interval(t2, t3)
    val interval3 = Interval(t0.plusDays(0.5), t1.plusDays(0.5))
    assert(interval0.contains(interval0))
    assert(false === interval0.contains(Interval(interval0.start, interval0.end.plusMillis(0.1))))
    assert(false === interval0.contains(interval1))
    assert(false === interval1.contains(interval2))
    assert(false === interval0.contains(interval2))
    assert(false === interval0.contains(interval3))

    val t0859 = TimeBuilder(2006, 1, 1, 8, 59, 0, 0)
    val t0900 = TimeBuilder(2006, 1, 1, 9, 0, 0, 0)
    val t0915 = TimeBuilder(2006, 1, 1, 9, 15, 0, 0)
    val t0930 = TimeBuilder(2006, 1, 1, 9, 30, 0, 0)
    val t0945 = TimeBuilder(2006, 1, 1, 9, 45, 0, 0)
    val t1000 = TimeBuilder(2006, 1, 1, 10, 0, 0, 0)
    val t1001 = TimeBuilder(2006, 1, 1, 10, 1, 0, 0)
    val t1400 = TimeBuilder(2006, 1, 1, 14, 0, 0, 0)
    val n2t = Interval(t0900, t1000)
    assert(n2t.contains(Interval(t0900, t1000)))
    assert(n2t.contains(Interval(t0900, t0930)))
    assert(n2t.contains(Interval(t0930, t1000)))
    assert(n2t.contains(Interval(t0915, t0945)))
    assert(n2t.contains(Interval(t0900, t0900)))
    //(otherStart before thisStart)
    assert(false === n2t.contains(Interval(t0859, t1000)))
    //(otherEnd after thisEnd)
    assert(false === n2t.contains(Interval(t0900, t1001)))
    //(otherStart equals thisEnd)
    assert(false === n2t.contains(Interval(t1000, t1000)))
    //Zero duration contains nothing.
    assert(false === Interval(t1400, t1400).contains(Interval(t1400, t1400)))
  }
  
  test("Intersect"){
    val t0800 = TimeBuilder(2006, 1, 1, 8, 0, 0, 0)
    val t0830 = TimeBuilder(2006, 1, 1, 8, 30, 0, 0)
    val t0900 = TimeBuilder(2006, 1, 1, 9, 0, 0, 0)
    val t0930 = TimeBuilder(2006, 1, 1, 9, 30, 0, 0)
    val t1000 = TimeBuilder(2006, 1, 1, 10, 0, 0, 0)
    val t1030 = TimeBuilder(2006, 1, 1, 10, 30, 0, 0)
    val t1100 = TimeBuilder(2006, 1, 1, 11, 0, 0, 0)
    val t1300 = TimeBuilder(2006, 1, 1, 13, 0, 0, 0)
    val t1400 = TimeBuilder(2006, 1, 1, 14, 0, 0, 0)
    val t1500 = TimeBuilder(2006, 1, 1, 15, 0, 0, 0)

    //We are going to intersect with this
    val startInterval = Interval(t0900, t1100)
    //Completely before
    val in = Interval(t0800, t0830) :: Interval(t1400, t1500) :: Interval(t0800, t0900) :: Interval(t1100, t1500) ::
      Interval(t1030, t1100) :: Interval(t0830, t0930) :: Nil
    val intervals = startInterval.intersect(in)
    assert(intervals.size == 2)
    assert(intervals.contains(Interval(t0900, t0930)))
    assert(intervals.contains(Interval(t1030, t1100)))
  }
  
  test("Overlaps"){
    val t0 = TimeBuilder(2006, 1, 1, 0, 0, 0, 0)
    val t1 = TimeBuilder(2006, 1, 2, 0, 0, 0, 0)
    val t2 = TimeBuilder(2006, 1, 3, 0, 0, 0, 0)
    val t3 = TimeBuilder(2006, 1, 4, 0, 0, 0, 0)
    //3 abutting intervals
    val interval0 = Interval(t0, t1)
    val interval1 = Interval(t1, t2)
    val interval2 = Interval(t2, t3)
    val interval3 = Interval(t0.plusDays(0.5), t1.plusDays(0.5))
    assert(false === interval0.overlaps(interval1))
    assert(false === interval1.overlaps(interval2))
    assert(false === interval0.overlaps(interval2))
    assert(interval0.overlaps(interval3))
    /**/
    //Use the same test cases from JODA Time
    val t0800 = TimeBuilder(2006, 1, 1, 8, 0, 0, 0)
    val t0830 = TimeBuilder(2006, 1, 1, 8, 30, 0, 0)
    val t0900 = TimeBuilder(2006, 1, 1, 9, 0, 0, 0)
    val t0930 = TimeBuilder(2006, 1, 1, 9, 30, 0, 0)
    val t1000 = TimeBuilder(2006, 1, 1, 10, 0, 0, 0)
    val t1030 = TimeBuilder(2006, 1, 1, 10, 30, 0, 0)
    val t1100 = TimeBuilder(2006, 1, 1, 11, 0, 0, 0)
    val t1300 = TimeBuilder(2006, 1, 1, 13, 0, 0, 0)
    val t1400 = TimeBuilder(2006, 1, 1, 14, 0, 0, 0)
    val t1500 = TimeBuilder(2006, 1, 1, 15, 0, 0, 0)
    val n2t = Interval(t0900, t1000)
    //Completely before
    assert(false === n2t.overlaps(Interval(t0800, t0830)))
    //Abuts before
    assert(false === n2t.overlaps(Interval(t0800, t0900)))
    assert(n2t.overlaps(Interval(t0800, t0930)))
    assert(n2t.overlaps(Interval(t0800, t1000)))
    assert(n2t.overlaps(Interval(t0800, t1100)))
    //Abuts before
    assert(false === n2t.overlaps(Interval(t0900, t0900)))
    assert(n2t.overlaps(Interval(t0900, t0930)))
    assert(n2t.overlaps(Interval(t0900, t1000)))
    assert(n2t.overlaps(Interval(t0900, t1100)))
    assert(n2t.overlaps(Interval(t0930, t0930)))
    assert(n2t.overlaps(Interval(t0930, t1000)))
    assert(n2t.overlaps(Interval(t0930, t1100)))
    //Abuts after
    assert(false === n2t.overlaps(Interval(t1000, t1000)))
    //Abuts after
    assert(false === n2t.overlaps(Interval(t1000, t1100)))
    //Completely after
    assert(false === n2t.overlaps(Interval(t1030, t1100)))
    val f2f = Interval(t1400, t1400)
    //Abuts before and after
    assert(false === f2f.overlaps(f2f))
    //Completely within
    assert(f2f.overlaps(Interval(t1300, t1500)))
  }
  
  test("Contains UTC Time"){
    val t0 = TimeBuilder(2006, 1, 1, 0, 0, 0, 0)
    val t1 = TimeBuilder(2006, 1, 2, 0, 0, 0, 0)
    val t2 = TimeBuilder(2006, 1, 3, 0, 0, 0, 0)
    val t3 = TimeBuilder(2006, 1, 4, 0, 0, 0, 0)
    //3 abutting intervals
    val interval0 = Interval(t1, t2)
    //Well before
    assert(false === interval0.contains(t0))
    //At end.  End points don't fall in interval since interval is open at end.
    assert(false === interval0.contains(t2))
    //Well after.
    assert(false === interval0.contains(t3))
    //At start point
    assert(true === interval0.contains(t1))
    //Something in the very middle.
    assert(true === interval0.contains(interval0.interpolate(0.5)))

    //JODA Tests
    val t0859 = TimeBuilder(2006, 1, 1, 8, 59, 0, 0)
    val t0900 = TimeBuilder(2006, 1, 1, 9, 0, 0, 0)
    val t0959 = TimeBuilder(2006, 1, 1, 9, 59, 0, 0)
    val t1000 = TimeBuilder(2006, 1, 1, 10, 0, 0, 0)
    val t1001 = TimeBuilder(2006, 1, 1, 10, 1, 0, 0)
    val t1400 = TimeBuilder(2006, 1, 1, 14, 0, 0, 0)
    val n2t = Interval(t0900, t1000)
    //(before start)
    assert(false === n2t.contains(t0859))
    assert(true === n2t.contains(t0900))
    assert(true === n2t.contains(t0959))
    //(equals end)
    assert(false === n2t.contains(t1000))
    //(after end)
    assert(false === n2t.contains(t1001))
    //(zero duration contains nothing)
    assert(false === Interval(t1400, t1400).contains(t1400))
  }
  
  test("Abuts"){
    val t0 = TimeBuilder(2006, 1, 1, 0, 0, 0, 0)
    val t1 = TimeBuilder(2006, 1, 2, 0, 0, 0, 0)
    val t2 = TimeBuilder(2006, 1, 3, 0, 0, 0, 0)
    val t3 = TimeBuilder(2006, 1, 4, 0, 0, 0, 0)
    //3 abutting intervals
    val interval0 = Interval(t0, t1)
    val interval1 = Interval(t1, t2)
    val interval2 = Interval(t2, t3)
    val interval3 = Interval(t0.plusDays(0.5), t1.plusDays(0.5))
    assert(true ===interval0.abuts(interval1))
    assert(true ===interval1.abuts(interval2))
    assert(false ===interval0.abuts(interval2))
    assert(false ===interval0.abuts(interval3))
  }

  test("isBefore"){
    val t0 = TimeBuilder(2006, 1, 1, 0, 0, 0, 0)
    val t1 = TimeBuilder(2006, 1, 2, 0, 0, 0, 0)
    val t2 = TimeBuilder(2006, 1, 3, 0, 0, 0, 0)
    val t3 = TimeBuilder(2006, 1, 4, 0, 0, 0, 0)
    //3 abutting intervals
    val interval0 = Interval(t1, t2)
    //Well before
    assert(false === interval0.isBefore(t0))
    //At end.  End points don't fall in interval since interval is open at end.
    assert(false === interval0.isBefore(t2))
    //Well after.
    assert(interval0.isBefore(t3))
    //At start point
    assert(false === interval0.isBefore(t1))
    //Something in the very middle.
    assert(false === interval0.isBefore(interval0.interpolate(0.5)))
  }

  test("Merge"){
    val t0 = TimeBuilder(2006, 1, 1, 0, 0, 0, 0)
    val t1 = TimeBuilder(2006, 1, 2, 0, 0, 0, 0)
    val t2 = TimeBuilder(2006, 1, 3, 0, 0, 0, 0)
    val t3 = TimeBuilder(2006, 1, 4, 0, 0, 0, 0)
    val interval0 = Interval(t0, t1)
    val interval1 = Interval(t1, t2)
    val interval2 = Interval(t2, t3)
    var merged = IntervalUtil.merge(Nil)
    assert(merged.size == 0, "Merging zero intervals should give an empty collection.")
    merged = IntervalUtil.merge(interval0 :: Nil)
    assert(merged.size == 1, "One interval should end up with one interval.")
    merged = IntervalUtil.merge(interval0 :: interval1 :: interval2 :: Nil)
    assert(merged.size == 1, "3 abutting intervals should give one interval.")
    merged = IntervalUtil.merge(interval0 :: interval2 :: Nil)
    assert(merged.size == 2, "2 non-overlapping intervals should give two intervals.")
  }

  test("Clip"){
    val t0700 = TimeBuilder(2006, 1, 1, 7, 0, 0, 0)
    val t0800 = TimeBuilder(2006, 1, 1, 8, 0, 0, 0)
    val t0900 = TimeBuilder(2006, 1, 1, 9, 0, 0, 0)
    val t0915 = TimeBuilder(2006, 1, 1, 9, 15, 0, 0)
    val t0930 = TimeBuilder(2006, 1, 1, 9, 30, 0, 0)
    val t0945 = TimeBuilder(2006, 1, 1, 9, 45, 0, 0)
    val t1000 = TimeBuilder(2006, 1, 1, 10, 0, 0, 0)
    val t1100 = TimeBuilder(2006, 1, 1, 11, 0, 0, 0)
    val t1200 = TimeBuilder(2006, 1, 1, 12, 0, 0, 0)
    val interval = Interval(t0900, t1000)
    //No clip cases.
    assert(interval.clip(Interval(t0700, t0800)).head.isEqual(interval), "Test completely before.  Shouldn't be clipped at all.")
    assert(interval.clip(Interval(t0800, t0900)).head.isEqual(interval), "Test abuts before.  Shouldn't be clipped at all.")
    assert(interval.clip(Interval(t1100, t1100)).head.isEqual(interval), "Test abuts after.  Shouldn't be clipped at all.")
    assert(interval.clip(Interval(t1100, t1200)).head.isEqual(interval), "Test completely after.  Shouldn't be clipped at all.")
    //Full clip cases.
    assert(interval.clip(Interval(t0900, t1000)).isEmpty, "Test equal.  Should be completely clipped.")
    assert(interval.clip(Interval(t0800, t1100)).isEmpty, "Test clipper contains clipee.  Should be completely clipped.")
    //Single result.
    val i0900to0930 = Interval(t0900, t0930)
    val i0930to1000 = Interval(t0930, t1000)
    val r0900to0930 = interval.clip(i0900to0930)
    val r0930to1000 = interval.clip(i0930to1000)
    //Should only get one result
    assert(r0900to0930.size == 1, "LHS clip size == 1.")
    assert(r0930to1000.size == 1, "RHS clip size == 1.")
    //In this case, clipping one half should get the other
    assert(r0900to0930.head.isEqual(i0930to1000), "LHS clip.  Should be subregion.")
    assert(r0930to1000.head.isEqual(i0900to0930), "RHS clip.  Should be subregion.")
    //Two ends.
    val i0915to0945 = Interval(t0915, t0945)
    val r0915to0945 = interval.clip(i0915to0945)
    assert(r0915to0945.size == 2, "Test clipper contains.  Should be completely clipped.")
    assert(r0915to0945.head.isEqual(Interval(t0900, t0915)), "LHS clip.  Should be subregion.")
    assert(r0915to0945.tail.head.isEqual(Interval(t0945, t1000)), "RHS clip.  Should be subregion.")
    //A collection of clips
    val clips = (t0700 until t0800) :: Nil
    interval.clip(clips)
    assert(interval.clip(clips).head.isEqual(Interval(t0900, t1000)), "Set of clips beforehand.  No change expected.")
  }

  test("Compare") {
    val t0 = TimeBuilder(2006, 1, 1, 0, 0, 0, 0)
    val t1 = TimeBuilder(2006, 1, 2, 0, 0, 0, 0)
    val t2 = TimeBuilder(2006, 1, 3, 0, 0, 0, 0)
    val interval0 = Interval(t0, t1)
    val interval1 = Interval(t1, t2)
    assert(interval0.compareTo(interval0) === 0)
    assert(interval0.compareTo(interval1) === -1)
    assert(interval1.compareTo(interval0) === 1)
  }

  test("subdivide") {
    val ti = TimeBuilder(2006, 1, 1, 0, 0, 0, 0)
    val tf = TimeBuilder(2006, 1, 2, 0, 0, 0, 0)
    val interval = ti until tf
    assert(interval.subdivide(0).length === 0)
    val x1 = interval.subdivide(1)
    assert(x1.length === 1)
    assert(x1.head == interval)
    val x2 = interval.subdivide(2)
    assert(x2.length === 2)
    assert(x2.head == (ti until interval.interpolate(0.5)))
    assert(x2.tail.head == (interval.interpolate(0.5) until tf))
  }
}
