/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.gmu.cds.multilab.affordances;

import edu.gmu.cds.multilab.active.Results;

/**
 *
 * @author Russell Thomas
 */
public interface EngageSearchable {
    public Results get(String name);
    public Results set(String name, Object obj);

}
