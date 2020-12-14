/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package View;

/**
 *
 * @author jpedro
 */
public class View {
       
    
        public void init(){
        System.out.println(
                "##########################################################\n"
             +  "##                 Servidor distrital                   ##\n"
             +  "##########################################################\n"
             +  "\n\n"
             +  "Welcome!\n"
        );
        
    }
      public void init(String district){
        System.out.println(
                "##########################################################\n"
             +  "##                 Servidor distrital                   ##\n"
             +  "##########################################################\n"
             +  "                   "+district+"\n"
             +  "\n\n"
             +  "Welcome!\n"
        );   
    } 
        
}
