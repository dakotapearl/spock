/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package spock.dataaccess.ejb;

import java.io.Serializable;

/**
 *
 * @author loren
 */
public class NewObject implements Serializable, NewObjectInterface {
    @Override
    public String getString() {
        return "NewObject's string";
    }
}
