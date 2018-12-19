package Image_Reader;

import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.imageio.ImageIO;

public class Test {
	
	public static void main(String[] args){

//		testImage_Reader();
		testColorConvert();
	}
	
	public static void testImage_Reader(){
	  String file_name = "world.jpg";
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
    
    cc.print_map();
    cc.print_colors(10);
    // 356,136 
    // 113,838 
    //  41,717 
    //  34,600 
    //  30,637 
    //  27,378 
    //  25,501 
    //  22,375 
    //  20,742 
    //  18,720 
	}
	
	public static void testColorConvert(){
	  int argb, red, green, blue;
	  red = 15;
	  green = 30;
	  blue = 50;
	  
	  // 0000 0000  0000 0000  0000 0000  0000 0000
	  argb = 0xff << 24; // 255 alpha
    argb += blue;
    argb += green << 8;
    argb += red << 16;
    
    blue = (argb << 24) >> 24;
    green = (argb << 16) >> 24;
    red = (argb << 8) >> 24;
    
    System.out.printf("Red: %d, Green %d, Blue %d\n", red, green, blue);
	}
	
	
}
