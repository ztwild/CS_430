package Image_Reader;

import java.awt.Color;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.imageio.ImageIO;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

import hw2.worker.ListDemo2;

public class GUI extends JFrame{
  private static List<Integer> list;
  
  
	public static void main(String[] args){
		init();
	  createFrame();
	}
	
	public static void init(){
	  String file_name = "test2.jpg";
    BufferedImage image = null;
    
    try {
      image = ImageIO.read(new File(file_name));
    } catch (IOException e) {
      System.err.println("Problem Opening File");
    }
    
    final byte[] pixels = ((DataBufferByte) image.getRaster().getDataBuffer()).getData();
    System.out.println("The length of the array is " + pixels.length);
    

    long startTime = System.currentTimeMillis();
    Color_Counter cc = new Color_Counter(pixels, 0, pixels.length);
    cc.run();
    long endTime = System.currentTimeMillis() - startTime;
    System.out.println("Image_Reader took " + endTime + " milliseconds.\n");
    list = cc.get_first(10);
    
	}
	

	
	public static void createFrame(){
		JFrame frame = new JFrame();
	  
    // give it a nonzero size
    frame.setSize(3000, 1500);
    
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setVisible(true);
    
    JTabbedPane tabbedPane = new JTabbedPane();
    for(int i = 0; i < list.size(); i++){
      JPanel panel = new JPanel();
      panel.setBackground(new Color(list.get(i), false));
      String title = "Tab " + i;
      tabbedPane.addTab(title, panel);
      
    }
    
    frame.add(tabbedPane);
	}
}
