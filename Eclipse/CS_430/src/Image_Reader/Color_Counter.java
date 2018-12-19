package Image_Reader;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveAction;
import java.util.concurrent.RecursiveTask;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.imageio.ImageIO;

public class Color_Counter extends RecursiveTask<Map<Integer, Integer>>{
  private final int start, length;
  private final byte[] pixels;
  private byte[] results;
  private static int RESOLUTION = 32;
  private static int DIFFERENCE = 4;
//  private static final int threshhold = 1000;
  private static final int threshhold = 3000000;
  private Map<Integer, Integer> colormap;
  
  public Color_Counter(byte[] pixels, int start, int length){
    this.pixels = pixels;
    this.start = start;
    this.length = length;
    this.colormap = new HashMap<>();
  }
  
  public Map<Integer, Integer> get_map(){
    return colormap;
  }
  
  public void run(){
    ForkJoinPool pool = new ForkJoinPool();
    Color_Counter task = new Color_Counter(pixels, start, length);
    colormap = pool.invoke(task);
  }
  
  
  @Override
  protected Map<Integer, Integer> compute() {
    if(length - start < threshhold){
      return computeDirectly();
    }
    
    int split = length / 2;
    Color_Counter task1 = new Color_Counter(pixels, start, split);
    Color_Counter task2 = new Color_Counter(pixels, start + split, length - split);
    task1.fork();
    task2.fork();
    Map<Integer, Integer> map1 = task1.join();
    Map<Integer, Integer> map2 = task2.join();
    
    merge(map1, map2);
    return colormap;
  }
  
  private Map<Integer, Integer> computeDirectly() {
    colormap = new HashMap<Integer, Integer>();
    int argb;
//    System.out.println("doesn't have alpha channel");
    final int pixelLength = 3;
    for (int pixel = start; pixel < length; pixel += pixelLength) {
      int blue, green, red;
      
      blue  = ((int) pixels[pixel] & 0xff) / RESOLUTION * RESOLUTION;
      green   = ((int) pixels[pixel + 1] & 0xff) / RESOLUTION * RESOLUTION;
      red   = ((int) pixels[pixel + 2] & 0xff) / RESOLUTION * RESOLUTION;
      
      argb = 0xff << 24; // 255 alpha
      argb += blue;
      argb += green << 8;
      argb += red << 16;
//      System.out.println("The color value is " + argb);
      add(argb);
    }
    return colormap;
  }
  
  
  public void merge(Map<Integer, Integer> map1, Map<Integer, Integer> map2){
    colormap = Stream.of(map1, map2)
        .map(Map::entrySet)
        .flatMap(Collection::stream)
        .collect(
            Collectors.toMap(
                Map.Entry::getKey,
                Map.Entry::getValue,
                Integer::sum
        ));
  }
  
  
  public void add(int colorValue){
    int count = 0;
    if(colormap.containsKey(colorValue)){
      count = colormap.get(colorValue);
    }
    colormap.put(colorValue, ++count);
  }
  
  public void print_map(){
    String str = colormap.toString();
    System.out.println(str);
  }
  
  public void print_list(List<Integer> list){
    String str = list.toString();
    System.out.println(str);
  }
  
//  public List<Integer> reduce_to(List<Integer> list, int n){
//    int ri, gi, bi, rj, gj, bj;
//    List<Integer> new_list = new ArrayList<>();
//    while(new_list.size() < n){
//      for(int i = 0; i < list.size(); i++){
//        int j;
//        int argb = list.get(i);
//        bi += (argb << 24) >> 24;
//        gi += (argb << 16) >> 24;
//        ri += (argb <<  8) >> 24;
//        for(j = 0; j < new_list.size(); j++){
//          
//        }
//      }
//      
//    }
//    
//    return list;
//  }
  
  public List<Integer> get_first(int n){
    List<Integer> list = null;
    list = colormap.keySet().stream()
        .sorted((key1, key2) -> colormap.get(key2).compareTo(colormap.get(key1)))
        .limit(n)
        .collect(Collectors.toList());
        
    return list;
  }
  
  
  public List<Integer> get_color_values(){
    List<Integer> list = null;
    list = colormap.keySet().stream()
        .collect(Collectors.toList());
    return list;
  }
  
  public List<Integer> get_color_count(){
    List<Integer> list = null;
    list = colormap.values().stream()
        .collect(Collectors.toList());
    
    return list;
  }
  
  public void print_colors(int n){
    String str;
    List<Integer> list = null;
    list = colormap.keySet().stream()
        .sorted((key1, key2) -> colormap.get(key2).compareTo(colormap.get(key1)))
        .limit(n)
        .collect(Collectors.toList());
        
    str = list.toString();
    System.out.println(str);
    
    System.out.print(" ");
    list.stream().forEach(e -> System.out.printf("%9d, ", colormap.get(e)));
  }
  
}
