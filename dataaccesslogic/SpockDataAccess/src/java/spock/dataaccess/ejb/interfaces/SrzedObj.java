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
public class SrzedObj implements SrzedObjInt, Serializable {

    protected String Prop = "goo";

    /**
     * Get the value of Prop
     *
     * @return the value of Prop
     */
    @Override
    public String getProp() {
        return Prop;
    }

    /**
     * Set the value of Prop
     *
     * @param Prop new value of Prop
     */
    @Override
    public void setProp(String Prop) {
        this.Prop = Prop;
    }

}
