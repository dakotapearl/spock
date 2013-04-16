/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package spock.dataaccess.ejb.interfaces.entities;

import java.io.Serializable;

/**
 *
 * @author loren
 */
public interface Metric extends Serializable {

    String getCodeURL();
    String getId();
    void setCodeURL(String CodeURL);
    void setId(String id);
    
}
