/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.gmu.cds.multilab.affordances;

import edu.gmu.cds.multilab.capabilities.Consuming;

/**
 *
 * @author Russell Thomas
 */
public class Consumable extends Affordance {
    private static final long serialVersionUID = 11;

    public Consumable(){
        Consuming c = new Consuming();
        this.relevantCapabilities.add(c);
    }
}
