package hw3.yahtzee;

public class PingReplyMessage extends AbstractMessage {
  
  private final boolean left;
  private final int index;
  private final int inrow;
  private final int total;
  private final Status status;

  public PingReplyMessage(int correlationId, Component sender, boolean left, int index, int inrow, int total, Status status){
    super(correlationId, sender);
    this.left = left;
    this.index = index;
    this.inrow = inrow;
    this.total = total;
    this.status = status;
    
  }
  
  public boolean fromLeft() { return left; }
  public int getIndex()     { return index; }
  public int getInrow()     { return inrow; }
  public int getTotal()     { return total; }
  public Status getStatus() { return status; }
  
  @Override
  public void dispatch(Component reciever){
    reciever.handlePingReply(this);
  }

  

}
