/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.gmu.multilab.artifact;

import sim.engine.SimState;

/**
 *
 * @author Russell Thomas
 */
public class PlanArtifact extends Artifact {
    private static final long serialVersionUID = 18; 
    public PlanArtifact(){
        
    }
    public PlanArtifact(final SimState state){
        super.setRNG(state.random);

    }
    
    public PlanArtifact(final SimState state, int serial){  
        super.setRNG(state.random);
        setID(serial);
    }
}