package com.todesking.unveil

// TODO: add query methods about types(isDoubleWord etc) for FrameUpdate
case class FrameItem(source: DataSource, data: Data) {
  def merge(rhs: FrameItem): FrameItem =
    FrameItem(source.merge(rhs.source), data.merge(rhs.data))
  def replaceDataBySource(src: DataSource.Single, to: Data): FrameItem =
    if(source == src) FrameItem(source, to) else this
}
