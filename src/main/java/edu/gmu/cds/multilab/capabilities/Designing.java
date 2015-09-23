/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.gmu.cds.multilab.capabilities;

/**
 *
 * @author Russell Thomas
 */

import edu.gmu.cds.multilab.interaction.*;
import edu.gmu.cds.multilab.processes.*;
import ec.util.MersenneTwisterFast;
import sim.util.*;


public class Designing extends Capability {
    DesignProcess myProcess;

    public Designing(){
        this.process = new DesignProcess(rng);
        this.myProcess = (DesignProcess)process;
    }
    
    
    public Designing(final MersenneTwisterFast r){
       this.myClass = this.getClass();
       super.rng = r;
       this.process = new DesignProcess(rng);
       this.myProcess = (DesignProcess)process;
    }
    
    public Bag doRandomDesign(){
        return myProcess.doRandomDesign();
    }
    
    
}
