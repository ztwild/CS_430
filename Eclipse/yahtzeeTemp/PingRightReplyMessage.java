package hw3.yahtzee;

public class PingRightReplyMessage extends AbstractMessage {
  private final int index;
  private final int total;
  
  public PingRightReplyMessage(int correlationId, Component sender, int index, int total) {
    super(correlationId, sender);
    this.index = index;
    this.total = total;
  }
  
  public int getIndex(){
    return index;
  }
  
  public int getTotal() {
    return total;
  }
  
  @Override
  public void dispatch(Component receiver){
    receiver.handlePingRightReply(this);
  }
  
}

