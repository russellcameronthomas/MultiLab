/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.gmu.cds.multilab.space;

/**
 *
 * @author Russell Thomas
 */

import edu.gmu.cds.multilab.interaction.*;

public abstract class Space extends Interactor implements EngageSpace {
    Object space = new Object();
    
    public Space(){
       this.space = new Object();
    }
    
    public Space(Object s){
       this.space = s;
    }
    
    public void setSpaceObject(Object s){
        space = s;
    }
    
    public Object getSpaceObject(){
        return space;
    }

}
