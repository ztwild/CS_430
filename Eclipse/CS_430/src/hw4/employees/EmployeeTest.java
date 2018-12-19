package hw4.employees;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import hw4.employees.EmployeeDatabase.Tuple;

/**
 * @author dwtj
 */
public class EmployeeTest {
  
    public static void main(String[] args) {

      final String filename = "src/hw4/employees/employees1000.txt";
      EmployeeDatabase db = new EmployeeDatabase(filename);

      // useful for testing, same list
      ArrayList<Employee> arr = EmployeeDatabase.parseDB(filename);
      
      /**
      double avg = db.averageSalaryForDepartment("Finance");
      System.out.println(avg);
      
      // do it the hard way, to check the result
      double total = 0;
      int count = 0;
      for (Employee e : arr)
      {
        if (e.getDepartment().equals("Finance"))
        {
          total += e.getSalary();
          count += 1;
        }
      }
      double expectedAvg = total / count;
      System.out.println("Expected " + expectedAvg);
      
      Map<String, Double> avgMap = db.averageSalariesByDepartment();
      System.out.println(avgMap.get("Finance"));
      System.out.println("Expected " + expectedAvg);

      // etc... try the other methods too
      
      List<Tuple<String, String>> productionList = db.listOfEmployeesInProduction();
//      System.out.println("Size of list is "+productionList.size());
//      productionList.forEach(e -> System.out.println(e.getSecond()));
      
      List<Tuple<String, Integer>> highestPaid = db.getHighestPaidEmployees(10);
//      System.out.println("There are "+highestPaid.size()+" that are the highest paid");
//      highestPaid.forEach(e->System.out.println(e.getFirst()));
      **/
      
      Map<String, List<String>> byPosition = db.listEmployeesByPosition();
      System.out.println("there are " + byPosition.size() + " positions");
//      byPosition.entrySet().stream().forEach(l->System.out.println(l.getValue().size()));
      
      // for part (c)
      Map<String, List<String>> byPosition2 = db.listEmployeesByPositionAlt();
      System.out.println("there are " + byPosition2.size() + " positions");
      byPosition2.entrySet().stream().forEach(l->System.out.println(l.getValue().size()));
      
      

    }
    

}
