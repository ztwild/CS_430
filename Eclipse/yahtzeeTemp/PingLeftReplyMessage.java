package hw3.yahtzee;

public class PingLeftReplyMessage extends AbstractMessage {
  private final int blocksInline;
  
  public PingLeftReplyMessage(int correlationId, Component sender, int blocksInline) {
    super(correlationId, sender);
    this.blocksInline = blocksInline;
  }
  
  public int getInline(){
    return blocksInline;
  }
  @Override
  public void dispatch(Component receiver){
    receiver.handlePingLeftReply(this);
  }
}
