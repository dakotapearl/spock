/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;

/**
 *
 * @author loren
 */
public interface SrzedObjInt extends Serializable {

    /**
     * Get the value of Prop
     *
     * @return the value of Prop
     */
    String getProp();

    /**
     * Set the value of Prop
     *
     * @param Prop new value of Prop
     */
    void setProp(String Prop);
    
}
